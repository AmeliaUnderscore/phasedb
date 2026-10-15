{-# LANGUAGE OverloadedStrings #-}

-- | User accounts, roles, and permission enforcement.
--
-- Follows the same "the database configures itself via its own data
-- model" convention already established by @^_CATALOG@ (routines)
-- and @^_CONFIG@ (tunables): user accounts live in an ordinary
-- special global, @^_USERS@, read and written through the storage
-- engine's normal 'readLive'/'commitSet' functions, NOT a separate
-- Haskell-level user database or config file.
--
-- == Storage shape
--
-- @
-- ^_USERS(username,"role")           = "none" | "read" | "write" | "admin"
-- ^_USERS(username,"passwordHash")   = <hex PBKDF2-SHA256 digest>
-- ^_USERS(username,"passwordSalt")   = <hex random salt>
-- ^_USERS(username,"denied",global)  = "1"   -- presence means denied; absence means allowed
-- @
--
-- == Two independent permission axes
--
-- 1. ROLE, a strict, linear hierarchy: 'RNone' (cannot even read) <
--    'RRead' (can read, cannot write) < 'RWrite' (can read and write)
--    < 'RAdmin' (can also run destructive maintenance and manage
--    @^_USERS@/@^_CONFIG@).
-- 2. PER-GLOBAL DENYLIST, independent of role: a user can be locked
--    out of a SPECIFIC global tree entirely (whole-tree, not
--    subtree, the team's own call on scope), regardless of what their
--    role would otherwise allow.
--
-- @^_USERS@ and @^_CONFIG@ are a hardcoded special case on top of
-- both of these: writable ONLY by 'RAdmin', unconditionally, this is
-- deliberately NOT expressed through the general per-global denylist
-- mechanism (which a non-admin's role could never grant them access
-- to anyway, but an admin *could*, in principle, misconfigure), so
-- that credential material can never become readable by a
-- misconfiguration. See 'checkGlobalAccess'.
module Phase.Auth
  ( -- * Roles
    Role (..)
  , roleFromText
  , roleToText

    -- * Password hashing
  , hashPassword
  , verifyPassword
  , generateSalt

    -- * User lookup
  , lookupUserRole
  , userExists
  , isGloballyDenied
  , hasAnyUser

    -- * User creation (bootstrap / admin management)
  , bootstrapCreateUser

    -- * Login
  , authenticateUser

    -- * Permission checks
  , checkReadAccess
  , checkWriteAccess
  , checkAdminAccess

    -- * Reserved-global write safeguards (subscript-aware; see below)
  , listUsernames
  , checkUsersKillSafety
  , checkUsersSetSafety
  , checkCatalogAccess
  ) where

import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Crypto.Random.Entropy as Entropy
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Time (UTCTime)
import Phase.AST (PValue (..), mkValue)
import Phase.Storage.Engine (StorageEngine, commitSet, orderLive, readLive)

-- --------------------------------------------------------------------
-- Roles
-- --------------------------------------------------------------------

-- | A strict, linear role hierarchy ("none/read/write/admin",
-- confirmed as a strict hierarchy, each level implies everything
-- below it). Deriving 'Ord' from this constructor order is what makes
-- 'checkReadAccess'/'checkWriteAccess' simple @role >= RRead@-style
-- comparisons rather than a bespoke implication table.
data Role = RNone | RRead | RWrite | RAdmin
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Parse a role from its stored textual form (case-insensitive, to
-- match the rest of the language's own keyword handling). Anything
-- unrecognized is treated as 'RNone', the safest possible fallback
-- for a corrupt or unexpected role value, rather than granting access
-- by default.
roleFromText :: Text -> Role
roleFromText t = case T.toLower (T.strip t) of
  "admin" -> RAdmin
  "write" -> RWrite
  "read"  -> RRead
  _       -> RNone

roleToText :: Role -> Text
roleToText RAdmin = "admin"
roleToText RWrite = "write"
roleToText RRead  = "read"
roleToText RNone  = "none"

-- --------------------------------------------------------------------
-- Password hashing (PBKDF2-SHA256, via cryptonite)
-- --------------------------------------------------------------------

-- | PBKDF2-SHA256 iteration count. 100,000 is a widely-used, current
-- (as of this writing) baseline for PBKDF2-SHA256, high enough to
-- meaningfully slow down offline brute-force, low enough to keep
-- interactive login latency unnoticeable.
pbkdf2Iterations :: Int
pbkdf2Iterations = 100000

-- | Output digest length in bytes.
pbkdf2OutputLength :: Int
pbkdf2OutputLength = 32

-- | Generate a random salt, hex-encoded for storage as a 'PValue',
-- everything in Phase's data model is text, so the salt is stored the
-- same way any other value would be, not as a separate binary field.
--
-- Uses 'Entropy.getEntropy' (from 'cryptonite', already a dependency
-- for the PBKDF2 hashing itself) rather than reading
-- @\/dev\/urandom@ directly: this codebase may run on Windows, which
-- has no such device file at all, and hand-rolling a Unix-only read
-- would silently break there. 'cryptonite' already ships correct,
-- platform-specific backends for exactly this (@\/dev\/urandom@ or
-- @getrandom()@ on Unix, @BCryptGenRandom@ on Windows), the right
-- move is to use the library's own portable primitive rather than
-- reimplement platform detection by hand.
generateSalt :: IO Text
generateSalt = do
  bytes <- Entropy.getEntropy 16 :: IO BS.ByteString
  pure (TE.decodeUtf8 (convertToBase Base16 bytes))

-- | Hash a password against a given (hex-encoded) salt, returning the
-- hex-encoded digest. Pure given the salt, 'generateSalt' is the
-- only part of this that needs randomness, so a plain, deterministic
-- function is enough here (and easier to test).
hashPassword :: Text -> Text -> Text
hashPassword password saltHex =
  case decodeHex saltHex of
    Nothing -> ""  -- malformed stored salt; verification will simply never match, which is the safe failure mode
    Just saltBytes ->
      let params = PBKDF2.Parameters { PBKDF2.iterCounts = pbkdf2Iterations, PBKDF2.outputLength = pbkdf2OutputLength }
          passwordBytes = TE.encodeUtf8 password
          digest = PBKDF2.fastPBKDF2_SHA256 params passwordBytes saltBytes :: BS.ByteString
      in TE.decodeUtf8 (convertToBase Base16 digest)

-- | Check a plaintext password attempt against a stored hash+salt
-- pair. Recomputes the hash from the attempt and the STORED salt,
-- then compares, never decrypts or reverses anything, since PBKDF2
-- is one-way by design.
--
-- Uses 'Data.ByteArray.constEq' (from the @memory@ package, already a
-- dependency) for the final comparison so that the response time does
-- not leak whether the candidate was "close" to the real hash.  The
-- @memory@ package is already listed in @phasedb-hs.cabal@ for its
-- encoding helpers; no new dependency is introduced.
verifyPassword :: Text -> Text -> Text -> Bool
verifyPassword attempt storedSalt storedHash =
  let candidate = TE.encodeUtf8 (hashPassword attempt storedSalt)
      stored    = TE.encodeUtf8 storedHash
  in constEq candidate stored

decodeHex :: Text -> Maybe BS.ByteString
decodeHex t =
  case convertFromBase Base16 (TE.encodeUtf8 t) of
    Left _  -> Nothing
    Right bs -> Just bs

-- --------------------------------------------------------------------
-- User lookup
-- --------------------------------------------------------------------

usersGlobalName :: Text
usersGlobalName = "_USERS"

-- | Does this username have ANY entry in @^_USERS@ at all? Used at
-- login time to distinguish "no such user" from "wrong password"
-- only insofar as both currently produce the same generic failure,
-- see the REPL login flow, which deliberately does NOT reveal which
-- of the two actually failed (a standard precaution against
-- username enumeration).
userExists :: Text -> StorageEngine -> Bool
userExists username eng =
  case readLive usersGlobalName [mkValue username, mkValue "role"] eng of
    Right _ -> True
    Left _  -> False

-- | Look up a user's role. A user with no role entry at all (or an
-- unrecognized value) is treated as 'RNone'.
lookupUserRole :: Text -> StorageEngine -> Role
lookupUserRole username eng =
  case readLive usersGlobalName [mkValue username, mkValue "role"] eng of
    Right pval -> roleFromText (unPValue pval)
    Left _     -> RNone

-- | Is this user denied access to a specific global tree, per
-- @^_USERS(username,"denied",globalName)@? Presence of ANY value
-- there (a $DATA-style existence check, not a value comparison) means
-- denied, this is deliberately a denylist, not an allowlist (per
-- the team's own call), and whole-tree, not subtree-scoped.
isGloballyDenied :: Text -> Text -> StorageEngine -> Bool
isGloballyDenied username globalName eng =
  case readLive usersGlobalName [mkValue username, mkValue "denied", mkValue globalName] eng of
    Right _ -> True
    Left _  -> False

-- | Does @^_USERS@ have ANY account defined at all? Used by the REPL
-- purely as a sanity signal, NOT as the trigger for running the
-- bootstrap flow, bootstrap is triggered by "this @.db@ file did not
-- exist before this process created it" (see @app/Main.hs@), so a
-- database that already exists but genuinely has no admin (a rarer,
-- arguably-corrupt state) is deliberately NOT auto-repaired by this
-- function; it exists for diagnostics, not as a policy decision
-- point.
hasAnyUser :: StorageEngine -> Bool
hasAnyUser eng = not (T.null (unPValue (orderLive usersGlobalName [] (mkValue "") eng)))

-- | Create a user account by writing DIRECTLY to the storage engine
-- (via 'commitSet'), bypassing 'Phase.State.writeRef' and its
-- permission checks entirely. This is deliberate and its use is
-- meant to stay narrow: @^_USERS@ is hardcoded admin-only (see the
-- module header), which means an ordinary write path can never create
-- the very FIRST admin account, there is no caller with an admin
-- role yet to satisfy that check. This function is that necessary
-- exception, used ONLY for:
--
--   1. The REPL's one-time bootstrap flow on a genuinely new database
--      file (creating the initial @SYSTEM@ admin, the team's own
--      design, chosen specifically so no database is ever left
--      wide open).
--   2. The test suite's own equivalent bootstrap step, so tests can
--      set up a real, ordinary admin account through the same
--      mechanism the REPL uses, rather than a separate shortcut that
--      would test different code than what actually runs in
--      production.
--
-- Every ordinary "admin creates another user" operation AFTER
-- bootstrap should go through the normal @SET ^_USERS(...)@ path
-- instead (which correctly enforces that only an already-established
-- admin may do so), this function has no such check by design, so it
-- must never be reachable from ordinary Phase language execution.
--
-- Attributes the resulting commit record to the new username itself
-- (e.g. "SYSTEM" creating "SYSTEM") rather than inventing a separate
-- bootstrap identity, there is no other meaningful acting user at
-- this point in a fresh database's history.
bootstrapCreateUser :: Text -> Text -> Role -> UTCTime -> StorageEngine -> IO (Either Text StorageEngine)
bootstrapCreateUser username password role now eng = do
  salt <- generateSalt
  let hash = hashPassword password salt
      writes =
        [ ([mkValue username, mkValue "role"], mkValue (roleToText role))
        , ([mkValue username, mkValue "passwordHash"], mkValue hash)
        , ([mkValue username, mkValue "passwordSalt"], mkValue salt)
        ]
  pure (foldl applyWrite (Right eng) writes)
  where
    applyWrite (Left err) _ = Left err
    applyWrite (Right e) (subs, val) =
      case commitSet username usersGlobalName subs val Nothing now e of
        Left storageErr -> Left (T.pack (show storageErr))
        Right (_, e')   -> Right e'

-- --------------------------------------------------------------------
-- Login
-- --------------------------------------------------------------------

-- | Verify a username/password attempt against @^_USERS@ for the
-- REPL's ordinary login flow. Deliberately returns the SAME 'False'
-- for "no such user" and "wrong password", this is a standard
-- precaution against username enumeration (an attacker probing logins
-- shouldn't be able to tell which part of their guess was wrong).
authenticateUser :: Text -> Text -> StorageEngine -> Bool
authenticateUser username password eng =
  case (,) <$> readLive usersGlobalName [mkValue username, mkValue "passwordSalt"] eng
           <*> readLive usersGlobalName [mkValue username, mkValue "passwordHash"] eng of
    Right (saltVal, hashVal) -> verifyPassword password (unPValue saltVal) (unPValue hashVal)
    Left _ -> False

-- --------------------------------------------------------------------
-- Permission checks
-- --------------------------------------------------------------------

-- | Is @^_USERS@ or @^_CONFIG@, hardcoded admin-only for BOTH read
-- and write, unconditionally, regardless of role or the per-global
-- denylist (which could otherwise, in principle, be misconfigured by
-- an admin to loosen access to credential material). See the module
-- header for why this is a hardcoded special case rather than
-- expressed through the general mechanism.
isHardcodedAdminOnlyGlobal :: Text -> Bool
isHardcodedAdminOnlyGlobal g = g == "_USERS" || g == "_CONFIG"

-- | Check whether a user may READ a given global. Returns 'Nothing'
-- on success, or 'Just' a human-readable reason on denial (the caller
-- wraps this into a real 'Phase.State.EvalError').
--
-- Order of checks: the hardcoded admin-only globals first (since no
-- role or per-global setting can ever override this), then role level
-- (must be at least 'RRead'), then the per-global denylist.
checkReadAccess :: Text -> Text -> StorageEngine -> Maybe Text
checkReadAccess username globalName eng
  | isHardcodedAdminOnlyGlobal globalName && lookupUserRole username eng < RAdmin =
      Just ("only admin accounts may access " <> globalName)
  | lookupUserRole username eng < RRead =
      Just "role does not permit reading"
  | isGloballyDenied username globalName eng =
      Just ("denied access to " <> globalName)
  | otherwise = Nothing

-- | Check whether a user may WRITE (SET/KILL/MERGE) a given global.
-- Same shape as 'checkReadAccess', requiring 'RWrite' instead of
-- 'RRead'.
checkWriteAccess :: Text -> Text -> StorageEngine -> Maybe Text
checkWriteAccess username globalName eng
  | isHardcodedAdminOnlyGlobal globalName && lookupUserRole username eng < RAdmin =
      Just ("only admin accounts may modify " <> globalName)
  | lookupUserRole username eng < RWrite =
      Just "role does not permit writing"
  | isGloballyDenied username globalName eng =
      Just ("denied access to " <> globalName)
  | otherwise = Nothing

-- | Check whether a user may run a destructive maintenance operation
-- (compaction). Requires 'RAdmin' specifically, not just enough role
-- to write ordinary data.
checkAdminAccess :: Text -> StorageEngine -> Maybe Text
checkAdminAccess username eng
  | lookupUserRole username eng < RAdmin = Just "this operation requires the admin role"
  | otherwise = Nothing

-- --------------------------------------------------------------------
-- Reserved-global write safeguards
-- --------------------------------------------------------------------
--
-- 'checkReadAccess'/'checkWriteAccess' above see only a global's NAME,
-- never which subscripts a particular statement touches. Two globals
-- need finer, subscript-aware rules a flat per-global gate cannot
-- express, so they get their own checks here. Wired in from
-- 'Phase.State' at the actual point of mutation, ALONGSIDE (never
-- instead of) the ordinary checks above.

-- | Every username currently present in @^_USERS@, in collation order.
listUsernames :: StorageEngine -> [Text]
listUsernames eng = go ""
  where
    go cur =
      let next = unPValue (orderLive usersGlobalName [] (mkValue cur) eng)
      in if T.null next then [] else next : go next

-- | Is this user currently the ONLY admin account?
isSoleAdmin :: Text -> StorageEngine -> Bool
isSoleAdmin username eng =
  lookupUserRole username eng == RAdmin
    && length (filter (== RAdmin) (map (`lookupUserRole` eng) (listUsernames eng))) <= 1

-- | Refuse a KILL on @^_USERS@ that would leave the database with no
-- admin account at all -- the safeguard against ending up with a
-- database nobody can administer.
--
-- Naming a specific user (or just their @\"role\"@ subscript) is
-- refused only when that user IS the sole admin; a broader kill (a
-- bare @KILL ^_USERS@, or naming nothing precise enough to identify
-- one user) is refused outright whenever ANY admin currently exists,
-- since it would remove them all at once. @^_USERS@ is hardcoded
-- admin-only for writes ('isHardcodedAdminOnlyGlobal'), so reaching
-- this function at all already means the acting user IS an admin --
-- this protects an admin from locking themselves out, not from a
-- lower-privileged user.
checkUsersKillSafety :: [PValue] -> StorageEngine -> Maybe Text
checkUsersKillSafety subs eng =
  case map unPValue subs of
    (username : _)
      | isSoleAdmin username eng ->
          Just ("cannot remove the last remaining admin account (" <> username <> ")")
      | otherwise -> Nothing
    [] | any (\u -> lookupUserRole u eng == RAdmin) (listUsernames eng) ->
          Just "cannot remove every user: at least one admin account must always remain"
       | otherwise -> Nothing

-- | Refuse a SET on @^_USERS(username,\"role\")@ that would demote the
-- sole remaining admin to anything else. Same "already an admin to
-- get here" reasoning as 'checkUsersKillSafety'.
checkUsersSetSafety :: [PValue] -> PValue -> StorageEngine -> Maybe Text
checkUsersSetSafety subs newVal eng =
  case map unPValue subs of
    [username, "role"]
      | roleFromText (unPValue newVal) /= RAdmin
      , isSoleAdmin username eng ->
          Just ("cannot demote the last remaining admin account (" <> username <> ")")
    _ -> Nothing

-- | The catalog global's name, and the subscript under an item that
-- records which role authored its CURRENT version (alongside
-- @\"VERSION\"@/@\"KIND\"@/@\"PARAMS\"@ in 'Phase.Catalog'). Duplicated
-- here rather than imported: 'Phase.Catalog' depends on this module
-- transitively through 'Phase.State', so the dependency can't run the
-- other way.
catalogGlobalName :: Text
catalogGlobalName = "_CATALOG"

catalogAuthorRoleSubscript :: Text
catalogAuthorRoleSubscript = "AUTHOR_ROLE"

-- | Is @^_CATALOG(ns,name)@'s current version authored by an admin?
-- An item with no marker at all (never defined, or defined before
-- this protection existed) is not admin-owned.
catalogItemIsAdminOwned :: Text -> Text -> StorageEngine -> Bool
catalogItemIsAdminOwned ns name eng =
  case readLive catalogGlobalName [mkValue ns, mkValue name, mkValue catalogAuthorRoleSubscript] eng of
    Right pval -> roleFromText (unPValue pval) == RAdmin
    Left _     -> False

-- | Refuse a non-admin's write into @^_CATALOG@ that would touch an
-- admin-authored item. Non-admin write access to the catalog is
-- otherwise intentional -- it is the only way a non-admin can define
-- their own routines/functions -- so this protects only what an admin
-- has already defined, not the catalog as a whole.
--
-- A path naming a specific @(namespace, name)@ is checked against
-- that one item. A path naming less than that (a whole namespace, or
-- the entire catalog) can reach many items at once, so for a
-- non-admin it is refused outright rather than walking the subtree to
-- check each one.
--
-- Deliberately NOT consulted by 'Phase.Catalog.defineItem' /
-- 'Phase.Catalog.revertItem' -- those run their own equivalent,
-- per-item check before writing (see their own docs), so the
-- sanctioned path through those functions is unaffected by this one.
-- This is wired in from 'Phase.State' only for a RAW SET/KILL/MERGE
-- reaching @^_CATALOG@ directly.
checkCatalogAccess :: Role -> [PValue] -> StorageEngine -> Maybe Text
checkCatalogAccess actingRole subs eng
  | actingRole >= RAdmin = Nothing
  | otherwise =
      case map unPValue subs of
        (ns : name : _)
          | catalogItemIsAdminOwned ns name eng ->
              Just ("^_CATALOG(" <> ns <> "," <> name <> ") was defined by an admin and can only be modified by an admin")
          | otherwise -> Nothing
        _ ->
          Just "modifying more than one ^_CATALOG(namespace,name) item at once requires the admin role"
