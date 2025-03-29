module Main where

import MyModel
import Buffer
import Constants

import Data.IORef
import Foreign
import Foreign.C.String
import Control.Concurrent.MVar
import Control.Monad (when, unless)
import Control.Concurrent (myThreadId)

-- Listener Callbacks
myGlobal :: Ptr SeatListener -> Ptr XdgWmBaseListener -> MVar Globals -> GlobalFn
myGlobal seatL xdgbL m _ reg n iface _ = do
    name <- peekCString iface
    case name of
         x | x == shmName -> do
             putStrLn "Found shm object to register"
             shm <- wlRegistryBind reg n wlShmInterface 1
             modifyMVar_ m (\g -> pure (g { unshm = castPtr shm }))
         x | x == seatName -> do
             putStrLn "Found seat object to register"
             seat <- wlRegistryBind reg n wlSeatInterface 1
             _ <- wlSeatAddListener (castPtr seat) seatL nullPtr
             modifyMVar_ m (\g -> pure (g { unseat = castPtr seat }))
         x | x == compositorName -> do
             putStrLn "Found compositor object to register"
             comp <- wlRegistryBind reg n wlCompositorInterface 1
             modifyMVar_ m (\g -> pure (g { uncomp = castPtr comp }))
         x | x == xdgShellName -> do
             putStrLn "Found xdg shell object to register"
             xdgb <- wlRegistryBind reg n xdgWmBaseInterface 1
             _ <- xdgWmBaseAddListener (castPtr xdgb) xdgbL nullPtr
             modifyMVar_ m (\g -> pure (g { unxdgwm = castPtr xdgb }))
         _ -> pure ()

myGlobalRemove :: GlobalRemoveFn
myGlobalRemove _ _ _ = putStrLn "TODO: global remove callback"

-- Global objects from the registry, captured in one object
-- since there is only one callback for the listener
data Globals = Globals
    { unshm :: Ptr Shm
    , unseat :: Ptr Seat
    , uncomp :: Ptr Compositor
    , unxdgwm :: Ptr XdgWmBase } deriving Show

mkRegistryListener :: MVar Globals -> IO (Ptr RegistryListener)
mkRegistryListener globals = do
    seatL <- mkWlSeatListener
    xdgbL <- mkXdgBaseListener
    gbFn <- registryListenerGlobalWrapper (myGlobal seatL xdgbL globals)
    gbrFn <- registryListenerGlobalRemoveWrapper myGlobalRemove
    new RegistryListener { global = gbFn, globalRemove =  gbrFn}

mySeatCapabilities :: CapabilitesFn
mySeatCapabilities _ s cap = do
    when (cap .&. 1 == 1) $ do
        _ <- wlSeatGetPointer s
        myid <- myThreadId
        putStrLn $ show myid ++ " TODO add pointer listener"

mySeatName :: SlNameFn
mySeatName _ _ n = putStrLn $ "seat name: " ++ show n

mkWlSeatListener :: IO (Ptr SeatListener)
mkWlSeatListener = do
    cp <- seatListenerCapabilitiesWrapper mySeatCapabilities
    np <- seatListenerNameWrapper mySeatName
    new SeatListener { slCapabilities = cp, slName = np }

myPing :: PingFn
myPing _ _ _ = pure ()

mkXdgBaseListener :: IO (Ptr XdgWmBaseListener)
mkXdgBaseListener = do
    pp <- xdgWmBaseListenerPingWrapper myPing
    new XdgWmBaseListener { xdgPing = pp }

myConfigure :: MVar Bool -> Ptr Surface -> ConfigureFn
myConfigure m wls _ xs s = do
    myid <- myThreadId
    putStrLn $ show myid ++ " How many configures?"
    xdgSurfaceAckConfigure xs s
    configured <- swapMVar m True
    unless configured $ wlSurfaceCommit wls

mkXdgSurfaceListener :: Ptr Surface -> MVar Bool -> IO (Ptr XdgSurfaceListener)
mkXdgSurfaceListener wlSurf mvar = do
    ptr <- xdgSurfaceListenerConfigureWrapper (myConfigure mvar wlSurf)
    new XdgSurfaceListener { xdgSurfConfigure = ptr }

-- A no-op can be used if you don't care about the callback function
noop :: IO ()
noop = mempty

markClose :: IORef Bool -> TopCloseFn
markClose ref _ _ = writeIORef ref False

mkXdgTopLevelListener :: IORef Bool -> IO (Ptr XdgTopLevelListener)
mkXdgTopLevelListener running = do
    np <- noopWrapper noop
    cp <- xdgTopCloseFnWrapper $ markClose running
    new XdgTopLevelListener { xdgTopConfigure = castFunPtr np
                            , xdgTopClose = cp
                            , xdgTopConfigureBounds = castFunPtr np
                            , xdgTopWmCapabilities = castFunPtr np }

-- Control flow helpers
whileM :: Monad m => m Bool -> m ()
whileM act = act >>= flip when (whileM act)

notConfigured :: Ptr Display -> MVar Bool -> IO Bool
notConfigured dpy mv = do
    nEvents <- wlDisplayDispatch dpy
    configured <- readMVar mv
    return $ nEvents /= -1 && not configured

notClosed :: Ptr Display -> IORef Bool -> IO Bool
notClosed dpy r = do
    nEvents <- wlDisplayDispatch dpy
    running <- readIORef r
    return $ nEvents /= -1 && running

main :: IO ()
main = do
    putStrLn "Hello, Wayland!"
    globals <- newMVar Globals  { unshm = nullPtr
                                , unseat = nullPtr
                                , uncomp = nullPtr
                                , unxdgwm = nullPtr }
    dpy <- wlDisplayConnect nullPtr
    reg <- wlDisplayGetRegistry dpy
    regL <- mkRegistryListener globals
    _ <- wlRegistryAddListener reg regL nullPtr
    -- Do a roundtrip, blocking for requests + events to go + come
    _ <- wlDisplayRountrip dpy
    gs <- readMVar globals
    print gs
    -- TODO replace with ConT for early "break"
    when (unshm gs == nullPtr || uncomp gs == nullPtr || unxdgwm gs == nullPtr) $ putStrLn "Things are going to break..."
    -- WL Surface is needed for an XDG Surface.  Commits happen against it
    -- TopLevel represents "normal" desktop windows (from xdg-desktop)
    surf <- wlCompositorCreateSurface (uncomp gs)
    xdgSurf <- xdgWmBaseGetXdgSurface (unxdgwm gs) surf
    xdgTop <- xdgSurfaceGetTopLevel xdgSurf
    -- XDG listeners
    confVar <- newMVar False
    running <- newIORef True
    xdgSL <- mkXdgSurfaceListener surf confVar
    xdgTL <- mkXdgTopLevelListener running
    _ <- xdgSurfaceAddListener xdgSurf xdgSL nullPtr
    _ <- xdgTopLevelAddListener xdgTop xdgTL nullPtr
    -- initial commit and wait for first configure
    wlSurfaceCommit surf
    whileM (notConfigured dpy confVar)
    -- Create WL buffer, attach to surface and commit the surface
    buff <- createBuffer (unshm gs)
    wlSurfaceAttach surf buff 0 0
    wlSurfaceCommit surf
    -- dispatch until toplevel close
    whileM (notClosed dpy running)
    -- Cleanup TODO
    wlDisplayDisconnect dpy
