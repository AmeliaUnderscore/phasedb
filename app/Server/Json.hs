{-# LANGUAGE OverloadedStrings #-}

-- | JSON encoding helpers for the PhaseDB HTTP API (§4.1, §4.2).
--
-- All JSON is hand-built with 'Data.Aeson' to keep the shapes exact and
-- avoid auto-derived instances that could silently add or rename fields.
module Server.Json
  ( -- * Response envelopes
    runEnvelope
  , errorEnvelope
  , serverErrorEnvelope
  , transportError

    -- * Individual response bodies
  , healthBody
  , sessionBody
  , sessionInfoBody
  , adminOkBody

    -- * Error code extraction
  , evalErrorCode
  , conditionJson

    -- * Request body parsing
  , parseExecuteBody
  , parseSessionBody
  , parseCompactBody
  , parseAddUserBody
  , ExecuteReq (..)
  , SessionReq (..)
  ) where

import Data.Aeson (Value (..), object, (.=), encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Phase.AST (PValue (..))
import Phase.State (Condition (..), EvalError (..), conditionMessage, formatError)
import Phase.Storage.Types (TxId)

-- --------------------------------------------------------------------
-- Response envelopes
-- --------------------------------------------------------------------

-- | Successful Phase-execution response (§4.1).
runEnvelope :: Text       -- ^ output
            -> Maybe Text -- ^ value (bare expression result)
            -> TxId
            -> Int        -- ^ elapsedMs
            -> Text       -- ^ requestId
            -> Maybe [Text] -- ^ registered names (for tag definitions)
            -> ByteString
runEnvelope output mVal lastTx elapsedMs reqId mRegistered =
  encode $ object $
    [ "ok"        .= True
    , "output"    .= output
    , "value"     .= maybe Null String mVal
    , "lastTx"    .= lastTx
    , "elapsedMs" .= elapsedMs
    , "requestId" .= reqId
    ] ++
    case mRegistered of
      Nothing    -> []
      Just names -> ["registered" .= names]

-- | Phase-level error response (§4.1).  HTTP 200, ok: false.
errorEnvelope :: EvalError -> TxId -> Int -> Text -> ByteString
errorEnvelope err lastTx elapsedMs reqId =
  encode $ object
    [ "ok"        .= False
    , "output"    .= ("" :: Text)
    , "error"     .= evalErrorJson err
    , "lastTx"    .= lastTx
    , "elapsedMs" .= elapsedMs
    , "requestId" .= reqId
    ]

-- | A Phase-run failure the server itself produces rather than the
-- engine (@TIMEOUT@, §4.1).  Same shape and HTTP 200 as 'errorEnvelope'.
serverErrorEnvelope :: Text   -- ^ code, e.g. \"TIMEOUT\"
                    -> Text   -- ^ message
                    -> TxId -> Int -> Text -> ByteString
serverErrorEnvelope code msg lastTx elapsedMs reqId =
  encode $ object
    [ "ok"        .= False
    , "output"    .= ("" :: Text)
    , "error"     .= object [ "code" .= code, "message" .= msg, "condition" .= Null ]
    , "lastTx"    .= lastTx
    , "elapsedMs" .= elapsedMs
    , "requestId" .= reqId
    ]

-- | Transport-level error (§4.2).  Non-200 status, no lastTx.
transportError :: Text   -- ^ code (e.g. "UNAUTHENTICATED")
               -> Text   -- ^ message
               -> Text   -- ^ requestId
               -> ByteString
transportError code msg reqId =
  encode $ object
    [ "ok"        .= False
    , "error"     .= object [ "code" .= code, "message" .= msg ]
    , "requestId" .= reqId
    ]

-- --------------------------------------------------------------------
-- Specific response bodies
-- --------------------------------------------------------------------

-- | @GET /v1/health@ response.
healthBody :: Text -> ByteString
healthBody reqId = encode $ object [ "status" .= ("ok" :: Text), "requestId" .= reqId ]

-- | @POST /v1/sessions@ response (201).
sessionBody :: Text      -- ^ Raw token (only time it is sent)
            -> UTCTime   -- ^ Absolute expiry
            -> Int       -- ^ Idle timeout seconds
            -> Text      -- ^ username
            -> Text      -- ^ role text
            -> Text      -- ^ namespace
            -> Text      -- ^ requestId
            -> ByteString
sessionBody token expiresAt idleTimeoutSecs user role ns reqId =
  encode $ object
    [ "token"              .= token
    , "expiresAt"          .= iso8601Show expiresAt
    , "idleTimeoutSeconds" .= idleTimeoutSecs
    , "user"               .= user
    , "role"               .= role
    , "namespace"          .= ns
    , "requestId"          .= reqId
    ]

-- | @GET /v1/sessions/current@ response.
sessionInfoBody :: Text -> Text -> Text -> UTCTime -> Int -> Text -> ByteString
sessionInfoBody user role ns expiresAt idleTimeoutSecs reqId =
  encode $ object
    [ "user"               .= user
    , "role"               .= role
    , "namespace"          .= ns
    , "expiresAt"          .= iso8601Show expiresAt
    , "idleTimeoutSeconds" .= idleTimeoutSecs
    , "requestId"          .= reqId
    ]

-- | Response for admin operations that just return ok + lastTx.
adminOkBody :: TxId -> Text -> ByteString
adminOkBody lastTx reqId =
  encode $ object [ "ok" .= True, "lastTx" .= lastTx, "requestId" .= reqId ]

-- --------------------------------------------------------------------
-- Error code extraction
-- --------------------------------------------------------------------

-- | Build the @error@ object from an 'EvalError'.
evalErrorJson :: EvalError -> Value
evalErrorJson err =
  let (code, msg, mCond) = evalErrorParts err
  in object [ "code"      .= code
            , "message"   .= msg
            , "condition" .= maybe Null conditionJson mCond
            ]

-- | Extract the code (text between angle brackets per §4.1), the full
-- formatted message, and a condition object for 'Signaled' errors.
evalErrorParts :: EvalError -> (Text, Text, Maybe Condition)
evalErrorParts (Signaled c) = (condName c, formatError (Signaled c), Just c)
evalErrorParts err          =
  let msg  = formatError err
      code = T.takeWhile (/= '>') (T.drop 1 msg)  -- text between < >
  in (code, msg, Nothing)

-- | Expose the error code of an 'EvalError' as a flat 'Text'.
evalErrorCode :: EvalError -> Text
evalErrorCode err = let (code, _, _) = evalErrorParts err in code

-- | Build the @condition@ JSON object for a signalled condition (§4.1).
conditionJson :: Condition -> Value
conditionJson c = object
  [ "name"   .= condName c
  , "fields" .= object [ AesonKey.fromText k .= unPValue v | (k, v) <- condFields c ]
  ]

-- --------------------------------------------------------------------
-- Request body parsing
-- --------------------------------------------------------------------

-- | Parsed body for @POST /v1/execute@ and @POST /v1/sessions/current/execute@.
data ExecuteReq = ExecuteReq
  { erCommand   :: !Text
  , erNamespace :: !Text       -- ^ "" if absent
  , erTimeoutMs :: !(Maybe Int)
  } deriving (Show)

-- | Parse an execute request body.
parseExecuteBody :: LBS.ByteString -> Either Text ExecuteReq
parseExecuteBody bs =
  case Aeson.decode bs :: Maybe Aeson.Value of
    Nothing  -> Left "malformed JSON"
    Just val -> case val of
      Object km ->
        case KM.lookup "command" km of
          Just (String cmd) ->
            Right ExecuteReq
              { erCommand   = cmd
              , erNamespace = case KM.lookup "namespace" km of
                                Just (String ns) -> ns
                                _                -> ""
              , erTimeoutMs = case KM.lookup "timeoutMs" km of
                                Just (Number n) -> Just (round n)
                                _               -> Nothing
              }
          _ -> Left "missing or non-string \"command\" field"
      _ -> Left "body must be a JSON object"

-- | Parsed body for @POST /v1/sessions@.
data SessionReq = SessionReq
  { srUsername :: !Text
  , srPassword :: !Text
  } deriving (Show)

-- | Parse a session-creation request body.
parseSessionBody :: LBS.ByteString -> Either Text SessionReq
parseSessionBody bs =
  case Aeson.decode bs :: Maybe Aeson.Value of
    Nothing  -> Left "malformed JSON"
    Just val -> case val of
      Object km ->
        case (KM.lookup "username" km, KM.lookup "password" km) of
          (Just (String u), Just (String p)) -> Right (SessionReq u p)
          _ -> Left "missing or non-string \"username\" or \"password\""
      _ -> Left "body must be a JSON object"

-- | Parse a @POST /v1/admin/compact@ body.
parseCompactBody :: LBS.ByteString -> Either Text Int
parseCompactBody bs =
  case Aeson.decode bs :: Maybe Aeson.Value of
    Nothing  -> Left "malformed JSON"
    Just val -> case val of
      Object km ->
        case KM.lookup "beforeTx" km of
          Just (Number n) -> Right (round n)
          _ -> Left "missing or non-number \"beforeTx\""
      _ -> Left "body must be a JSON object"

-- | Parse a @POST /v1/admin/users@ body.
parseAddUserBody :: LBS.ByteString -> Either Text (Text, Text, Text)
parseAddUserBody bs =
  case Aeson.decode bs :: Maybe Aeson.Value of
    Nothing  -> Left "malformed JSON"
    Just val -> case val of
      Object km ->
        case (KM.lookup "username" km, KM.lookup "password" km, KM.lookup "role" km) of
          (Just (String u), Just (String p), Just (String r)) -> Right (u, p, r)
          _ -> Left "missing or non-string field in {username, password, role}"
      _ -> Left "body must be a JSON object"
