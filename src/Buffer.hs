module Buffer
    ( createBuffer )
where

import Foreign.Marshal.Utils (copyBytes)
import Foreign.C
import Foreign
import Control.Monad (when)
import MMAP
import qualified Data.ByteString as B

import Shm
import MyModel (Shm, Buffer, wlShmCreatePool, wlShmPoolCreateBuffer)

-- convenience
fi :: forall a b. (Integral a, Num b) => a -> b
fi = fromIntegral

-- Despite using WLM_SHM_FORMAT_ARGB8888, you gotta flip it turnways (BGRA)
-- example for reference
myImage :: [Word8]
myImage = concat $ replicate (fi width * fi height) [0x00, 0xff, 0xff, 0xff]

width = 128
height = 128
stride = 4 * width
size = height * stride

-- Enum from C headers
wlShmFormatARGB8888 = 0

createBuffer :: Ptr Shm -> IO (Ptr Buffer)
createBuffer shm = do
    -- Allocate a shared memory file with the right size
    fd <- createShmFile size
    when (fd < 0) $ putStrLn "Failed to make SHM"
    -- Map the shared memory file
    shmData <- mmap nullPtr (fi size) (protRead <> protWrite)
                    (mkMmapFlags mapShared mempty) (fi fd) 0
    shmPool <- wlShmCreatePool shm fd size
    buffer <- wlShmPoolCreateBuffer shmPool 0 width height stride wlShmFormatARGB8888
    -- Now that we've mapped the file and created the wl_buffer, we no longer
    -- need to keep the file descriptor opened
    closeFD fd
    -- Copy pixels into our shared memory file
    bytes <- B.readFile "cat.bgra"
    withArray (B.unpack bytes) $ \img ->
        copyBytes (castPtr shmData) img (fi size)
    pure buffer
