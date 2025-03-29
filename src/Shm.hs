{-# LANGUAGE CApiFFI #-}
module Shm where

import Foreign
import Foreign.C

foreign import capi unsafe "shm.h create_shm_file" createShmFile :: CInt -> IO CInt
foreign import capi unsafe "unistd.h close" closeFD :: CInt -> IO ()
