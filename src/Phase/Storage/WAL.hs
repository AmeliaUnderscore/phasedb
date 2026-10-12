{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary serialization and crash recovery for PhaseDB's persistent
-- record stream (the @.db@ file).
--
-- == Why binary, not text
--
-- The previous version of this module used a hand-rolled, delimiter-
-- based text format (fields separated by spaces, brackets, and pipes,
-- with backslash-escaping for a handful of characters). That format
-- had a real, confirmed bug: a SUBSCRIPT containing a literal @]@
-- character (e.g. @SET ^ORDERS("A]B")=1@) desynchronized the reader's
-- delimiter scanning and produced either a parse failure or silently
-- wrong data -- @]@ was never in the escape table, because it was
-- being used as a structural delimiter, not user content, and nobody
-- had reason to think a legitimate VALUE could ever collide with it.
-- Any delimiter-based scheme has this class of bug lurking in it: the
-- moment user data can contain a byte that also means something
-- structurally, the format is ambiguous. This gets categorically
-- worse once arbitrary binary blob content (Part XI) is stored --
-- there is no character or byte sequence that's safe to reserve as a
-- delimiter against genuinely arbitrary bytes.
--
-- The fix is the standard one for this whole class of problem (it's
-- what protocol buffers, MessagePack, SQLite's own WAL format, and
-- most real database log formats do): LENGTH-PREFIX every
-- variable-length field instead of terminating it with a delimiter.
-- Reading a length-prefixed field never inspects the field's own
-- bytes to decide where it ends, so no byte value -- @]@, a null byte,
-- raw JPEG data, anything -- can ever be misread as structure. See
-- 'putText'/'getText' below for where this actually happens.
--
-- == File format
--
-- @
-- [4 bytes magic "PHDB"] [1 byte format version]
-- then a sequence of:
--   [Word32 recordLength] [recordLength bytes: an encoded WalRecord]
-- @
--
-- A record whose declared length would run past the actual remaining
-- file size (a partial write from a crash mid-append) is treated as
-- the end of valid data and dropped, exactly as the previous text
-- format tolerated a malformed trailing line -- this is NOT a new
-- failure mode, just the same tolerance applied correctly to binary
-- framing instead of attempting to line-split partial binary bytes
-- (which would be far more likely to produce garbage than a clean
-- "this doesn't fit" check).
--
-- == Checkpoints live in this same file
--
-- 'Phase.Storage.Types.WalCheckpoint' is a record type like any
-- other, interleaved with ordinary write records in append order.
-- There is no more separate @.snap@ file -- seeing a checkpoint record
-- while reading is how 'Phase.Storage.Engine' finds a resume point
-- for both ordinary startup (find the LAST checkpoint) and time-travel
-- (find the last checkpoint AT OR BEFORE the target moment) without
-- reading the whole file. See 'Phase.Storage.Engine' for how the
-- decision of WHICH checkpoint to resume from is made -- this module
-- only handles encoding/decoding/framing.
module Phase.Storage.WAL
  ( -- * Serialization
    encodeWalOp
  , decodeWalOp
  , encodeWalRecord
  , decodeWalRecord
  , walRecordByteSize

    -- * File Persistence
  , appendWalRecord
  , appendWalRecords
  , readWalRecords
  , recoverWalFile
  , dbFileHeader
  ) where

import Control.Exception (catch, IOException)
import Control.Monad (when)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Int (Int64)
import Data.Word (Word8, Word32, Word64)
import System.Directory (doesFileExist, getFileSize)
import System.IO (IOMode (..), hSetFileSize, withBinaryFile)

import Phase.AST (PValue (..), mkValue)
import Phase.Storage.Sync (syncFile)
import Phase.Storage.Types (StorageError (..), TxId, WalOp (..), WalRecord (..))

-- --------------------------------------------------------------------
-- File header
-- --------------------------------------------------------------------

-- | Magic bytes + format version written at the very start of a fresh
-- @.db@ file. Lets 'readWalRecords' fail fast and clearly on a
-- corrupt or wrong-format file, rather than attempting to interpret
-- arbitrary bytes as record-length-prefixed data (which -- since a
-- 'Word32' length prefix can decode to almost anything -- could
-- otherwise "succeed" in nonsensical ways instead of failing
-- cleanly).
--
-- Version history:
--   1 -- initial binary format (length-prefixed records, checkpoints)
--   2 -- 'WalCommit' gained a username field (the team's user/role
--        system); per Astra, breaking compatibility with existing
--        (test-only) databases across this bump is fine, so there is
--        no dual-format reader, a v1 file simply won't have this
--        version byte and will be treated as unreadable.
dbFileHeader :: BS.ByteString
dbFileHeader = BS.pack ([0x50, 0x48, 0x44, 0x42] ++ [formatVersion])
  where
    formatVersion :: Word8
    formatVersion = 2

-- --------------------------------------------------------------------
-- Primitive field encoders/decoders
-- --------------------------------------------------------------------

-- | Encode 'Text' as a length-prefixed UTF-8 byte string: an 8-byte
-- big-endian length, followed by exactly that many bytes. THIS is the
-- primitive that makes arbitrary content safe -- 'getText' never scans
-- the content itself for a terminator, it only ever trusts the
-- length that was written.
putText :: Text -> P.Put
putText t =
  let bytes = TE.encodeUtf8 t
  in P.putWord64be (fromIntegral (BS.length bytes)) <> P.putByteString bytes

getText :: G.Get Text
getText = do
  len <- G.getWord64be
  bytes <- G.getByteString (fromIntegral len)
  -- 'decodeUtf8'' rather than 'decodeUtf8': invalid UTF-8 in a damaged
  -- record must surface as an ordinary decode failure (so recovery can
  -- classify it), not as a lazily-thrown exception from pure code.
  case TE.decodeUtf8' bytes of
    Left err -> fail ("invalid UTF-8 in text field: " ++ show err)
    Right t  -> pure t

-- | Encode a list of 'PValue's (used for subscript paths) as a
-- count-prefixed sequence of length-prefixed fields -- no delimiter
-- between items, so a subscript value containing any byte sequence
-- (including one that would have collided with the old format's own
-- delimiters) round-trips exactly.
putPValueList :: [PValue] -> P.Put
putPValueList vs = do
  P.putWord64be (fromIntegral (length vs))
  mapM_ (putText . unPValue) vs

getPValueList :: G.Get [PValue]
getPValueList = do
  n <- G.getWord64be
  mapM (const (mkValue <$> getText)) [1 .. n]

-- | Encode a 'Maybe TxId': a 1-byte tag (0 = 'Nothing', 1 = 'Just'),
-- followed by the 'Word64' only when present.
putMaybeTxId :: Maybe TxId -> P.Put
putMaybeTxId Nothing = P.putWord8 0
putMaybeTxId (Just tx) = P.putWord8 1 <> P.putWord64be tx

getMaybeTxId :: G.Get (Maybe TxId)
getMaybeTxId = do
  tag <- G.getWord8
  case tag of
    0 -> pure Nothing
    1 -> Just <$> G.getWord64be
    _ -> fail ("invalid Maybe TxId tag: " ++ show tag)

-- | Encode a 'UTCTime' as microseconds since the Unix epoch (a signed
-- 64-bit integer -- signed, not unsigned, so a timestamp before 1970
-- is at least representable rather than wrapping, even though Phase
-- itself never produces one). Fixed-width, so no framing ambiguity;
-- chosen over any textual timestamp format purely for compactness and
-- to avoid re-introducing ad hoc text parsing for something that's
-- naturally numeric.
putTimestamp :: UTCTime -> P.Put
putTimestamp t =
  let micros = round (utcTimeToPOSIXSeconds t * 1000000) :: Integer
  in P.putInt64be (fromIntegral micros)

getTimestamp :: G.Get UTCTime
getTimestamp = do
  micros <- G.getInt64be
  pure (posixSecondsToUTCTime (fromIntegral micros / 1000000))

-- --------------------------------------------------------------------
-- WalOp / WalRecord encoding
-- --------------------------------------------------------------------

-- | Encode a 'WalOp' to its binary representation (a 1-byte tag
-- followed by tag-specific fields -- see the module header's "File
-- format" section for the overall framing this sits inside).
encodeWalOp :: WalOp -> P.Put
encodeWalOp (WalSet g subs val mExp) = do
  P.putWord8 0
  putText g
  putPValueList subs
  putText (unPValue val)
  putMaybeTxId mExp
encodeWalOp (WalKill g subs) = do
  P.putWord8 1
  putText g
  putPValueList subs
encodeWalOp (WalKillSubtree g subs) = do
  P.putWord8 2
  putText g
  putPValueList subs
encodeWalOp (WalCommit tx ts user) = do
  P.putWord8 3
  P.putWord64be tx
  putTimestamp ts
  putText user
encodeWalOp (WalRollback tx) = do
  P.putWord8 4
  P.putWord64be tx
encodeWalOp (WalCheckpoint tx ts entries) = do
  P.putWord8 5
  P.putWord64be tx
  putTimestamp ts
  P.putWord64be (fromIntegral (length entries))
  mapM_ putEntry entries
  where
    putEntry (g, subs, val, lastTx) = do
      putText g
      putPValueList subs
      putText (unPValue val)
      P.putWord64be lastTx

-- | Decode a 'WalOp' from its binary representation.
decodeWalOp :: G.Get WalOp
decodeWalOp = do
  tag <- G.getWord8
  case tag of
    0 -> do
      g <- getText
      subs <- getPValueList
      val <- mkValue <$> getText
      mExp <- getMaybeTxId
      pure (WalSet g subs val mExp)
    1 -> WalKill <$> getText <*> getPValueList
    2 -> WalKillSubtree <$> getText <*> getPValueList
    3 -> WalCommit <$> G.getWord64be <*> getTimestamp <*> getText
    4 -> WalRollback <$> G.getWord64be
    5 -> do
      tx <- G.getWord64be
      ts <- getTimestamp
      n <- G.getWord64be
      entries <- mapM (const getEntry) [1 .. n]
      pure (WalCheckpoint tx ts entries)
    _ -> fail ("invalid WalOp tag: " ++ show tag)
  where
    getEntry = do
      g <- getText
      subs <- getPValueList
      val <- mkValue <$> getText
      lastTx <- G.getWord64be
      pure (g, subs, val, lastTx)

-- | Encode a complete 'WalRecord'.
encodeWalRecord :: WalRecord -> P.Put
encodeWalRecord (WalRecord tx op) = do
  P.putWord64be tx
  encodeWalOp op

-- | Decode a complete 'WalRecord'.
decodeWalRecord :: G.Get WalRecord
decodeWalRecord = WalRecord <$> G.getWord64be <*> decodeWalOp

-- | The number of bytes a 'WalRecord' would actually occupy on disk,
-- INCLUDING its own 4-byte length-prefix framing (i.e. exactly what
-- 'appendWalRecord' would add to the file's size). Used by
-- 'Phase.Storage.Engine''s automatic checkpointing to track "bytes
-- added since the last checkpoint" against a configured threshold,
-- without needing to actually touch disk just to measure a size.
walRecordByteSize :: WalRecord -> Int64
walRecordByteSize record = BL.length (frameRecord record)

-- --------------------------------------------------------------------
-- File-level framing and persistence
-- --------------------------------------------------------------------

-- | Encode one record as @[Word32 length][bytes]@, the unit
-- 'appendWalRecord'/'readWalRecords' operate on.
frameRecord :: WalRecord -> BL.ByteString
frameRecord record =
  let body = P.runPut (encodeWalRecord record)
      len = fromIntegral (BL.length body) :: Word32
  in P.runPut (P.putWord32be len) <> body

-- | Append a single 'WalRecord' to a log file on disk, creating the
-- file (with its header) if it doesn't exist yet.
appendWalRecord :: FilePath -> WalRecord -> IO ()
appendWalRecord path record = appendWalRecords path [record]

-- | Append several 'WalRecord's in ONE file operation and one write,
-- then @fsync@. A whole transaction's records (its writes plus the
-- trailing 'WalCommit') are always passed together so they reach disk
-- as a unit; 'recoverWalFile' relies on that to treat anything after
-- the last commit as an incomplete transaction. Returning from this
-- function means the data is durable, not merely handed to the OS.
appendWalRecords :: FilePath -> [WalRecord] -> IO ()
appendWalRecords _ [] = pure ()
appendWalRecords path records = do
  needsHeader <- fileNeedsHeader path
  let payload = mconcat (map frameRecord records)
      chunk = if needsHeader then BL.fromStrict dbFileHeader <> payload else payload
  -- Binary mode: on Windows a text-mode handle may translate newline
  -- bytes, which would corrupt the record stream. Closing the handle
  -- flushes Haskell's buffer to the OS; 'syncFile' then forces it to
  -- disk (see "Phase.Storage.Sync" for why this goes by path).
  withBinaryFile path AppendMode (\h -> BL.hPut h chunk)
  syncFile path
  where
    -- Size check only: this used to read the ENTIRE file on every
    -- append just to see whether it was empty.
    fileNeedsHeader p =
      ((== 0) <$> getFileSize p) `catch` (\(_ :: IOException) -> pure True)

-- --------------------------------------------------------------------
-- Scanning and recovery
-- --------------------------------------------------------------------

-- | How a scan over the record area ended.
data ScanEnd
  = EndClean                  -- ^ consumed every byte
  | EndTorn                   -- ^ a trailing partial record (crash mid-append)
  | EndCorrupt !Int64 String  -- ^ a fully-framed record that will not decode,
                              --   with MORE data after it; byte offset (relative
                              --   to the first record) and decoder message

-- | Walk length-prefixed records. Each result carries the byte offset
-- just PAST that record (relative to the start of the record area), so
-- callers can cut the file at a record boundary.
--
-- A record that will not decode but is the very last thing in the file
-- is classified as a torn tail (e.g. a zero-filled extension left by a
-- crash), since nothing valuable can follow it. One with data after it
-- is genuine corruption, and the scan stops there rather than guessing.
scanRecords :: BL.ByteString -> ([(WalRecord, Int64)], ScanEnd)
scanRecords = go 0 []
  where
    go :: Int64 -> [(WalRecord, Int64)] -> BL.ByteString -> ([(WalRecord, Int64)], ScanEnd)
    go off acc bs
      | BL.null bs = (reverse acc, EndClean)
      | BL.length bs < 4 = (reverse acc, EndTorn)
      | otherwise =
          case G.runGetOrFail G.getWord32be bs of
            Left _ -> (reverse acc, EndTorn)
            Right (rest, _, len) ->
              let len' = fromIntegral len :: Int64
              in if BL.length rest < len'
                   then (reverse acc, EndTorn)
                   else
                     let (recordBytes, afterRecord) = BL.splitAt len' rest
                         next = off + 4 + len'
                     in case G.runGetOrFail decodeWalRecord recordBytes of
                          Right (_, _, r) -> go next ((r, next) : acc) afterRecord
                          Left (_, _, msg)
                            | BL.null afterRecord -> (reverse acc, EndTorn)
                            | otherwise           -> (reverse acc, EndCorrupt off msg)

-- | Header classification for the first bytes of a file.
data HeaderStatus
  = HeaderOk
  | HeaderTorn         -- ^ empty, or a strict prefix of the header (crash while creating the file)
  | HeaderBad Text     -- ^ not a Phase file, or an unsupported format version

checkHeader :: BS.ByteString -> HeaderStatus
checkHeader content
  | BS.null content = HeaderTorn
  | BS.length content < hlen =
      if content `BS.isPrefixOf` dbFileHeader
        then HeaderTorn
        else HeaderBad "not a Phase database file (bad magic bytes)"
  | BS.take hlen content == dbFileHeader = HeaderOk
  | BS.take (hlen - 1) content == BS.take (hlen - 1) dbFileHeader =
      HeaderBad ("unsupported format version "
                 <> T.pack (show (BS.index content (hlen - 1)))
                 <> " (this build reads version "
                 <> T.pack (show (BS.last dbFileHeader)) <> ")")
  | otherwise = HeaderBad "not a Phase database file (bad magic bytes)"
  where
    hlen = BS.length dbFileHeader

-- | Records whose arrival completes a unit of durable work. A
-- transaction is appended as [writes..., 'WalCommit'] in one batch, so
-- the file is always a sequence of complete units.
closesUnit :: WalOp -> Bool
closesUnit (WalCommit {})     = True
closesUnit (WalRollback {})   = True
closesUnit (WalCheckpoint {}) = True
closesUnit _                  = False

-- | Cut a file down to exactly @len@ bytes and make that durable.
truncateWal :: FilePath -> Int64 -> IO ()
truncateWal path len =
  do
    withBinaryFile path ReadWriteMode (\h -> hSetFileSize h (fromIntegral len))
    syncFile path

-- | Open-time recovery. Reads the file, repairs it, and returns the
-- records that survive:
--
--   * A missing file, an empty file, or a partially written header
--     yields no records (a partial header is cut back to empty).
--   * A wrong magic or an unsupported format version is an ERROR, not
--     "no data", so a foreign or newer file is never appended to or
--     overwritten.
--   * A trailing partial record (crash mid-append) is TRUNCATED off the
--     file. Merely ignoring it is not enough: appending after the
--     partial bytes would make the reader swallow the start of the next
--     record and silently lose committed data.
--   * Records after the last 'WalCommit'/'WalCheckpoint' belong to a
--     transaction whose commit never reached disk. They are dropped
--     AND truncated, so a torn transaction is never replayed as
--     committed and its transaction id is not confused with the next
--     real one.
--   * Genuine mid-file corruption is an ERROR ('StorageCorruptWal').
--     Recovery will not guess, and will not delete the valid data that
--     follows a damaged record.
recoverWalFile :: FilePath -> IO (Either StorageError [WalRecord])
recoverWalFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      content <- BS.readFile path
      let hlen = BS.length dbFileHeader
      case checkHeader content of
        HeaderBad why -> pure (Left (StorageCorruptWal why))
        HeaderTorn -> do
          when (not (BS.null content)) (truncateWal path 0)
          pure (Right [])
        HeaderOk -> do
          let body = BL.fromStrict (BS.drop hlen content)
              (scanned, end) = scanRecords body
          case end of
            EndCorrupt off msg ->
              pure (Left (StorageCorruptWal
                ("record at byte offset " <> T.pack (show (fromIntegral hlen + off))
                 <> " is corrupt: " <> T.pack msg)))
            _ -> do
              let unitEnds = [ o | (WalRecord _ op, o) <- scanned, closesUnit op ]
                  keepEnd = if null unitEnds then 0 else last unitEnds
                  kept = [ r | (r, o) <- scanned, o <= keepEnd ]
                  newLen = fromIntegral hlen + keepEnd :: Int64
                  fileLen = fromIntegral (BS.length content) :: Int64
              when (newLen < fileLen) (truncateWal path newLen)
              pure (Right kept)

-- | Read all valid 'WalRecord's from a log file, in file order, for
-- the read-only consumers (time travel, compaction). Stops at the
-- first incomplete or undecodable record, and yields nothing for a
-- missing file or an unrecognised header. Unlike 'recoverWalFile' this
-- never modifies the file.
readWalRecords :: FilePath -> IO [WalRecord]
readWalRecords path = do
  content <- BS.readFile path `catch` (\(_ :: IOException) -> pure BS.empty)
  case checkHeader content of
    HeaderOk ->
      let (scanned, _) = scanRecords (BL.fromStrict (BS.drop (BS.length dbFileHeader) content))
      in pure (map fst scanned)
    _ -> pure []
