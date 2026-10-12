{-# LANGUAGE CPP #-}

-- | Portable "make this file's contents durable" (@fsync@ /
-- @FlushFileBuffers@).
--
-- 'base' has no fsync, and the OS-specific packages differ:
-- @unix@ does not build on Windows and @Win32@ does not build
-- elsewhere. The cabal file selects the right one per platform and this
-- module hides the difference behind one function.
--
-- It works on a PATH, not a 'System.IO.Handle', on purpose: converting a
-- Haskell handle to an OS descriptor depends on GHC's I/O manager
-- internals, which differ between POSIX and Windows. Both fsync and
-- FlushFileBuffers act on the file rather than on the particular
-- descriptor that wrote the data, so writing through a normal 'Handle',
-- closing it (which flushes Haskell's buffer to the OS), and then
-- syncing by path gives the guarantee we need.
module Phase.Storage.Sync
  ( syncFile
  ) where

import Control.Exception (bracket)

#if defined(mingw32_HOST_OS)
import Data.Bits ((.|.))
import qualified System.Win32.File as W
#else
import System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.Unistd (fileSynchronise)
#endif

-- | Force everything written so far to the named (existing) file out to
-- stable storage. The caller must have closed, or at least flushed, its
-- own write handle first.
--
-- Not covered: syncing the parent directory after a file is first
-- created.
syncFile :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
syncFile path =
  bracket
    (W.createFile path W.gENERIC_WRITE
       (W.fILE_SHARE_READ .|. W.fILE_SHARE_WRITE)
       Nothing W.oPEN_EXISTING W.fILE_ATTRIBUTE_NORMAL Nothing)
    W.closeHandle
    W.flushFileBuffers
#else
syncFile path =
  bracket
#if MIN_VERSION_unix(2,8,0)
    (openFd path WriteOnly defaultFileFlags)
#else
    -- unix <2.8 has an extra 'Maybe FileMode' argument (the
    -- creation permission bits for a NEW file -- irrelevant here,
    -- 'syncFile' only ever opens a file that already exists) between
    -- 'OpenMode' and 'OpenFileFlags'; 2.8 folded it into
    -- 'OpenFileFlags' instead and dropped the argument. Found by
    -- actually compiling this against GHC 9.4.7's bundled unix-2.7.3
    -- -- untested on a unix>=2.8 toolchain.
    (openFd path WriteOnly Nothing defaultFileFlags)
#endif
    closeFd
    fileSynchronise
#endif
