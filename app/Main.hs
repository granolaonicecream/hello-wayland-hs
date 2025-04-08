module Main where

import MyModel
import Buffer
import Constants

import Data.IORef
import Foreign
import Control.Concurrent.MVar
import Control.Monad (when, unless)
import Control.Concurrent (myThreadId)
import Data.Maybe (fromJust, isJust)
import Data.Coerce (coerce)

import qualified Graphics.Wayland.Client as WLC

type GlobalFn' = WLC.Registry -> Word -> String -> Word -> IO ()

-- Listener Callbacks
myGlobal :: WLC.SeatListener -> Ptr XdgWmBaseListener -> MVar Globals -> GlobalFn'
myGlobal seatL xdgbL m reg n name _ = do
    case name of
         x | x == shmName -> do
             putStrLn "Found shm object to register"
             shm <- WLC.registryBindShm reg n name 1
             modifyMVar_ m (\g -> pure (g { unshm = shm }))
         x | x == seatName -> do
             putStrLn "Found seat object to register"
             seat <- WLC.registryBindSeat reg n name 1
             _ <- WLC.seatSetListener seat seatL
             modifyMVar_ m (\g -> pure (g { unseat = seat }))
         x | x == compositorName -> do
             putStrLn "Found compositor object to register"
             comp <- WLC.registryBindCompositor reg n name 1
             modifyMVar_ m (\g -> pure (g { uncomp = comp }))
         x | x == xdgShellName -> do
             putStrLn "Found xdg shell object to register"
             xdgb <- wlRegistryBind (coerce reg) (fromIntegral n) xdgWmBaseInterface 1
             _ <- xdgWmBaseAddListener (castPtr xdgb) xdgbL nullPtr
             modifyMVar_ m (\g -> pure (g { unxdgwm = castPtr xdgb }))
         _ -> pure ()

myGlobalRemove :: WLC.Registry -> Word -> IO ()
myGlobalRemove _ _ = putStrLn "TODO: global remove callback"

-- Global objects from the registry, captured in one object
-- since there is only one callback for the listener
data Globals = Globals
    { unshm :: WLC.Shm
    , unseat :: WLC.Seat
    , uncomp :: WLC.Compositor
    , unxdgwm :: Ptr XdgWmBase } deriving Show

mkRegistryListener :: MVar Globals -> IO (WLC.RegistryListener)
mkRegistryListener globals = do
    let seatL = mkWlSeatListener
    xdgbL <- mkXdgBaseListener
    let gbFn = (myGlobal seatL xdgbL globals)
    return WLC.RegistryListener { registryGlobal = gbFn, registryGlobalRemove =  myGlobalRemove}

mySeatCapabilities :: WLC.Seat -> Word -> IO ()
mySeatCapabilities _ cap = do
    when (cap .&. 1 == 1) $ do
--         _ <- wlSeatGetPointer s
        myid <- myThreadId
        putStrLn $ show myid ++ " TODO add pointer listener"

mySeatName :: WLC.Seat -> String -> IO ()
mySeatName _  n = putStrLn $ "seat name: " ++ show n

mkWlSeatListener :: WLC.SeatListener
mkWlSeatListener = WLC.SeatListener { seatCapabilities = mySeatCapabilities , seatName = mySeatName }

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

notConfigured :: WLC.Display -> MVar Bool -> IO Bool
notConfigured dpy mv = do
    nEvents <- WLC.displayDispatch dpy
    configured <- readMVar mv
    return $ isJust nEvents && not configured

notClosed :: WLC.Display -> IORef Bool -> IO Bool
notClosed dpy r = do
    nEvents <- WLC.displayDispatch dpy
    running <- readIORef r
    return $ isJust nEvents && running

main :: IO ()
main = do
    putStrLn "Hello, Wayland!"
    globals <- newMVar Globals  { unshm = WLC.Shm nullPtr
                                , unseat = WLC.Seat nullPtr
                                , uncomp = WLC.Compositor nullPtr
                                , unxdgwm = nullPtr }
    dpy <- fromJust <$> WLC.displayConnect
    reg <- WLC.displayGetRegistry dpy
    regL <- mkRegistryListener globals
    _ <- WLC.registrySetListener reg regL
    -- Do a roundtrip, blocking for requests + events to go + come
    _ <- WLC.displayRoundtrip dpy
    gs <- readMVar globals
    print gs
    -- TODO replace with ConT for early "break"
    when (unshm gs == (coerce nullPtr) || uncomp gs == (coerce nullPtr) || unxdgwm gs == (coerce nullPtr)) $ putStrLn "Things are going to break..."
    -- WL Surface is needed for an XDG Surface.  Commits happen against it
    -- TopLevel represents "normal" desktop windows (from xdg-desktop)
    surf <- WLC.compositorCreateSurface (uncomp gs)
    xdgSurf <- xdgWmBaseGetXdgSurface (unxdgwm gs) (coerce surf)
    xdgTop <- xdgSurfaceGetTopLevel xdgSurf
    -- XDG listeners
    confVar <- newMVar False
    running <- newIORef True
    xdgSL <- mkXdgSurfaceListener (coerce surf) confVar
    xdgTL <- mkXdgTopLevelListener running
    _ <- xdgSurfaceAddListener xdgSurf xdgSL nullPtr
    _ <- xdgTopLevelAddListener xdgTop xdgTL nullPtr
    -- initial commit and wait for first configure
    WLC.surfaceCommit surf
    whileM (notConfigured dpy confVar)
    -- Create WL buffer, attach to surface and commit the surface
    buff <- createBuffer (coerce (unshm gs))
    WLC.surfaceAttach surf (Just buff) 0 0
    WLC.surfaceCommit surf
    -- dispatch until toplevel close
    whileM (notClosed dpy running)
    -- Cleanup TODO
    WLC.displayDisconnect dpy
