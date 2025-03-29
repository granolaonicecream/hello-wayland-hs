module Constants where

import Foreign (Ptr, peek)
import Foreign.C (peekCString)
import System.IO.Unsafe (unsafeDupablePerformIO)

import MyModel

-- Interfaces are const in headers, so should be safe(?)
interfaceName :: Ptr Interface -> String
interfaceName i = unsafeDupablePerformIO $ peekCString . MyModel.ifaceName =<< peek i

shmName :: String
shmName = interfaceName wlShmInterface

seatName :: String
seatName = interfaceName wlSeatInterface

compositorName :: String
compositorName = interfaceName wlCompositorInterface

xdgShellName :: String
xdgShellName = interfaceName xdgWmBaseInterface
