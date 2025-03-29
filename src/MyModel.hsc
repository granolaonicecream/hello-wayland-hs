module MyModel where

#include <wayland-client.h>
#include <wayland-util.h>
#include "xdg-shell.h"

import Foreign
import Foreign.C
import Foreign.C.String

-- Most WL types are opaque pointers from the Client's perspective
data Display
data Registry
data Proxy
data Shm
data ShmPool
data Buffer
data Seat
data Compositor
data XdgWmBase
data Pointer
data Surface
data XdgSurface
data XdgTopLevel
data WlArray
-- Extern const structs in protocol headers (represent the protocol interfaces)
data Interface = Interface { ifaceName :: CString }
-- Models for Listener structs, comprised of callbacks (see protocol "events")
data RegistryListener = RegistryListener
    { global :: FunPtr (GlobalFn)
    , globalRemove :: FunPtr (GlobalRemoveFn) }

data SeatListener = SeatListener
    { slCapabilities :: FunPtr (CapabilitesFn)
    , slName :: FunPtr (SlNameFn) }

data XdgWmBaseListener = XdgWmBaseListener
    { xdgPing :: FunPtr (PingFn) }

data XdgSurfaceListener = XdgSurfaceListener
    { xdgSurfConfigure :: FunPtr (ConfigureFn) }

data XdgTopLevelListener = XdgTopLevelListener
    { xdgTopConfigure :: FunPtr (TopConfigureFn)
    , xdgTopClose :: FunPtr (TopCloseFn)
    , xdgTopConfigureBounds :: FunPtr (TopConfBoundsFn)
    , xdgTopWmCapabilities :: FunPtr (TopWmCapabilities) }

-- Function type aliases that we need to make FunPtr from
-- These are all callbacks for the various Listeners
type GlobalFn = Ptr () -> Ptr Registry -> CUInt -> Ptr CChar -> CUInt -> IO ()
type GlobalRemoveFn = Ptr () -> Ptr Registry -> CUInt -> IO ()

-- void (*capabilities)(void *data, struct wl_seat *wl_seat, uint32_t capabilities);
-- void (*name)(void *data, struct wl_seat *wl_seat, const char *name);
-- void (*ping)(void *data, struct xdg_wm_base *xdg_wm_base, uint32_t serial);
-- void (*configure)(void *data, struct xdg_surface *xdg_surface, uint32_t serial);
type CapabilitesFn = Ptr () -> Ptr Seat -> CUInt -> IO ()
type SlNameFn = Ptr () -> Ptr Seat -> CString -> IO ()
type PingFn = Ptr () -> Ptr XdgWmBase -> CUInt -> IO ()
type ConfigureFn = Ptr () -> Ptr XdgSurface -> CUInt -> IO ()

-- void (*configure)(void *data, struct xdg_toplevel *xdg_toplevel, int32_t width, int32_t height, struct wl_array *states);
-- void (*close)(void *data, struct xdg_toplevel *xdg_toplevel);
-- void (*configure_bounds)(void *data,struct xdg_toplevel *xdg_toplevel,int32_t width,int32_t height);
-- void (*wm_capabilities)(void *data,struct xdg_toplevel *xdg_toplevel,struct wl_array *capabilities);
type TopConfigureFn = Ptr () -> Ptr XdgTopLevel -> CInt -> CInt -> Ptr WlArray -> IO ()
type TopCloseFn = Ptr () -> Ptr XdgTopLevel -> IO ()
type TopConfBoundsFn = Ptr () -> Ptr XdgTopLevel -> CInt -> CInt -> IO ()
type TopWmCapabilities = Ptr () -> Ptr XdgTopLevel -> Ptr WlArray -> IO ()

-- Storable instances
instance Storable Interface where
    sizeOf _ = #{size struct wl_interface}
    alignment _ = #{alignment struct wl_interface}
    peek ptr = do
        n <- #{peek struct wl_interface, name} ptr
        return (Interface n)
    poke ptr (Interface n) = do
        #{poke struct wl_interface, name} ptr n

instance Storable RegistryListener where
    sizeOf _ = #{size struct wl_registry_listener}
    alignment _ = #{alignment struct wl_registry_listener}
    peek ptr = do
        g <- #{peek struct wl_registry_listener, global} ptr
        gr <- #{peek struct wl_registry_listener, global_remove} ptr
        return (RegistryListener g gr)
    poke ptr (RegistryListener g gr) = do
        #{poke struct wl_registry_listener, global} ptr g
        #{poke struct wl_registry_listener, global_remove} ptr gr

instance Storable SeatListener where
    sizeOf _ = #{size struct wl_seat_listener}
    alignment _ = #{alignment struct wl_seat_listener}
    peek ptr = do
        c <- #{peek struct wl_seat_listener, capabilities} ptr
        n <- #{peek struct wl_seat_listener, name} ptr
        return (SeatListener c n)
    poke ptr (SeatListener c n) = do
        #{poke struct wl_seat_listener, capabilities} ptr c
        #{poke struct wl_seat_listener, name} ptr n

instance Storable XdgWmBaseListener where
    sizeOf _ = #{size struct xdg_wm_base_listener}
    alignment _ = #{alignment struct xdg_wm_base_listener}
    peek ptr = do
        p <- #{peek struct xdg_wm_base_listener, ping} ptr
        return (XdgWmBaseListener p)
    poke ptr (XdgWmBaseListener p) = do
        #{poke struct xdg_wm_base_listener, ping} ptr p

instance Storable XdgSurfaceListener where
    sizeOf _ = #{size struct xdg_surface_listener}
    alignment _ = #{alignment struct xdg_surface_listener}
    peek ptr = do
        c <- #{peek struct xdg_surface_listener, configure} ptr
        return (XdgSurfaceListener c)
    poke ptr (XdgSurfaceListener c) = do
        #{poke struct xdg_surface_listener, configure} ptr c

instance Storable XdgTopLevelListener where
    sizeOf _ = #{size struct xdg_toplevel_listener}
    alignment _ = #{alignment struct xdg_toplevel_listener}
    peek ptr = do
        conf <- #{peek struct xdg_toplevel_listener, configure} ptr
        cl <- #{peek struct xdg_toplevel_listener, close} ptr
        cfb <- #{peek struct xdg_toplevel_listener, configure_bounds} ptr
        wmc <- #{peek struct xdg_toplevel_listener, wm_capabilities} ptr
        return (XdgTopLevelListener conf cl cfb wmc)
    poke ptr (XdgTopLevelListener conf cl cfb wmc) = do
        #{poke struct xdg_toplevel_listener, configure} ptr conf
        #{poke struct xdg_toplevel_listener, close} ptr cl
        #{poke struct xdg_toplevel_listener, configure_bounds} ptr cfb
        #{poke struct xdg_toplevel_listener, wm_capabilities} ptr wmc

-- From client core, NOT protocol
foreign import ccall unsafe "wl_display_connect" wlDisplayConnect :: Ptr CChar -> IO (Ptr Display)
foreign import ccall unsafe "wl_display_disconnect" wlDisplayDisconnect :: Ptr Display -> IO ()
foreign import ccall "wl_proxy_get_version" wlProxyGetVersion :: Ptr Proxy -> IO (CUInt)
foreign import ccall unsafe "wl_proxy_add_listener" wlProxyAddListener :: Ptr Proxy -> Ptr (FunPtr () -> ()) -> Ptr () -> IO (CInt)
-- MUST be safe since haskell callbacks are exercised before this returns
foreign import ccall safe "wl_display_roundtrip" wlDisplayRountrip :: Ptr Display -> IO (CInt)
foreign import ccall safe "wl_display_dispatch" wlDisplayDispatch :: Ptr Display -> IO (CInt)

-- Protocol interface references, needed for identifying globals
foreign import ccall "&wl_shm_interface" wlShmInterface :: Ptr Interface
foreign import ccall "&wl_seat_interface" wlSeatInterface :: Ptr Interface
foreign import ccall "&wl_compositor_interface" wlCompositorInterface :: Ptr Interface
foreign import ccall "&xdg_wm_base_interface" xdgWmBaseInterface :: Ptr Interface
foreign import ccall "&wl_pointer_interface" wlPointerInterface :: Ptr Interface
foreign import ccall "&wl_surface_interface" wlSurfaceInterface :: Ptr Interface
foreign import ccall "&xdg_surface_interface" xdgSurfaceInterface :: Ptr Interface
foreign import ccall "&xdg_toplevel_interface" xdgTopLevelInterface :: Ptr Interface
foreign import ccall "&wl_shm_pool_interface" wlShmPoolInterface :: Ptr Interface
foreign import ccall "&wl_buffer_interface" wlBufferInferface :: Ptr Interface
foreign import ccall "&wl_registry_interface" wlRegistryInterface :: Ptr Interface


-- No varargs support in FFI bindings... Need a better way to handle
-- wl_proxy_marshal_flags is the core function for libwayland client
foreign import ccall unsafe "wl_proxy_marshal_flags" wlProxyMarshalFlags :: Ptr Proxy -> CUInt -> Ptr Interface -> CUInt -> CUInt -> IO (Ptr Proxy)
foreign import ccall unsafe "wl_proxy_marshal_flags" wlProxyMarshalFlags' :: Ptr Proxy -> CUInt -> Ptr Interface -> CUInt -> CUInt -> CUInt -> CString -> CUInt -> Ptr () -> IO (Ptr Proxy)
foreign import ccall unsafe "wl_proxy_marshal_flags" wlProxyMarshalFlags6 :: Ptr Proxy -> CUInt -> Ptr Interface -> CUInt -> CUInt -> Ptr () -> IO (Ptr Proxy)
foreign import ccall unsafe "wl_proxy_marshal_flags" wlProxyMarshalFlags7 :: Ptr Proxy -> CUInt -> Ptr Interface -> CUInt -> CUInt -> Ptr () -> Ptr Surface -> IO (Ptr Proxy)
foreign import ccall unsafe "wl_proxy_marshal_flags" wlProxyMarshalFlags6' :: Ptr Proxy -> CUInt -> Ptr Interface -> CUInt -> CUInt -> CUInt -> IO (Ptr Proxy)
foreign import ccall unsafe "wl_proxy_marshal_flags" wlProxyMarshalFlagsShmPool :: Ptr Proxy -> CUInt -> Ptr Interface -> CUInt -> CUInt -> Ptr () -> CInt -> CInt -> IO (Ptr Proxy)
foreign import ccall unsafe "wl_proxy_marshal_flags" wlProxyMarshalFlagsCreateBuffer :: Ptr Proxy -> CUInt -> Ptr Interface -> CUInt -> CUInt -> Ptr () -> CInt -> CInt -> CInt -> CInt -> CUInt -> IO (Ptr Proxy)
foreign import ccall unsafe "wl_proxy_marshal_flags" wlProxyMarshalFlagsBufferAttach :: Ptr Proxy -> CUInt -> Ptr Interface -> CUInt -> CUInt -> Ptr Buffer -> CInt -> CInt -> IO (Ptr Proxy)


-- can't do static inline functions, reimplement in haskell or write a C wrapper function and ffi to that'
wlDisplayGetRegistry :: Ptr Display -> IO (Ptr Registry)
wlDisplayGetRegistry dp = do
    let dProxy = castPtr dp
    version <- wlProxyGetVersion dProxy
    castPtr <$> wlProxyMarshalFlags dProxy 1 wlRegistryInterface version 0

wlRegistryAddListener :: Ptr Registry -> Ptr RegistryListener -> Ptr () -> IO (CInt)
wlRegistryAddListener rp rlp d = wlProxyAddListener (castPtr rp) (castPtr rlp) d

wlRegistryBind :: Ptr Registry -> CUInt -> Ptr Interface -> CUInt -> IO (Ptr ())
wlRegistryBind rp n ip version = do
    iname <- ifaceName <$> peek ip
    ptr <- wlProxyMarshalFlags' (castPtr rp) 0 ip version 0 n iname version nullPtr
    return (castPtr ptr)

wlSeatAddListener :: Ptr Seat -> Ptr SeatListener -> Ptr () -> IO (CInt)
wlSeatAddListener s sl d = wlProxyAddListener (castPtr s) (castPtr sl) d

wlSeatGetPointer :: Ptr Seat -> IO (Ptr Pointer)
wlSeatGetPointer s = do
    let proxy = castPtr s
    version <- wlProxyGetVersion proxy
    castPtr <$> wlProxyMarshalFlags6 proxy 0 wlPointerInterface version 0 nullPtr

xdgWmBaseAddListener :: Ptr XdgWmBase -> Ptr XdgWmBaseListener -> Ptr () -> IO (CInt)
xdgWmBaseAddListener x xl d = wlProxyAddListener (castPtr x) (castPtr xl) d

wlCompositorCreateSurface :: Ptr Compositor -> IO (Ptr Surface)
wlCompositorCreateSurface cp = do
    let proxy = castPtr cp
    version <- wlProxyGetVersion proxy
    castPtr <$> wlProxyMarshalFlags6 proxy 0 wlSurfaceInterface version 0 nullPtr

xdgWmBaseGetXdgSurface :: Ptr XdgWmBase -> Ptr Surface -> IO (Ptr XdgSurface)
xdgWmBaseGetXdgSurface xp sp = do
    let proxy = castPtr xp
    version <- wlProxyGetVersion proxy
    castPtr <$> wlProxyMarshalFlags7 proxy 2 xdgSurfaceInterface version 0 nullPtr sp

xdgSurfaceGetTopLevel :: Ptr XdgSurface -> IO (Ptr XdgTopLevel)
xdgSurfaceGetTopLevel xsp = do
    let proxy = castPtr xsp
    version <- wlProxyGetVersion proxy
    castPtr <$> wlProxyMarshalFlags6 proxy 1 xdgTopLevelInterface version 0 nullPtr

xdgSurfaceAddListener :: Ptr XdgSurface -> Ptr XdgSurfaceListener -> Ptr () -> IO (CInt)
xdgSurfaceAddListener xsp xslp d = wlProxyAddListener (castPtr xsp) (castPtr xslp) d

xdgSurfaceAckConfigure :: Ptr XdgSurface -> CUInt -> IO ()
xdgSurfaceAckConfigure xsp s = do
    let proxy = castPtr xsp
    version <- wlProxyGetVersion proxy
    wlProxyMarshalFlags6' proxy 4 nullPtr version 0 s
    pure ()

xdgTopLevelAddListener :: Ptr XdgTopLevel -> Ptr XdgTopLevelListener -> Ptr () -> IO (CInt)
xdgTopLevelAddListener t l d = wlProxyAddListener (castPtr t) (castPtr l) d

wlSurfaceCommit :: Ptr Surface -> IO ()
wlSurfaceCommit s = do
    let proxy = castPtr s
    version <- wlProxyGetVersion proxy
    wlProxyMarshalFlags proxy 6 nullPtr version 0
    pure ()

wlShmCreatePool :: Ptr Shm -> CInt -> CInt -> IO (Ptr ShmPool)
wlShmCreatePool s fd size = do
    let proxy = castPtr s
    version <- wlProxyGetVersion proxy
    castPtr <$> wlProxyMarshalFlagsShmPool proxy 0 wlShmPoolInterface version 0 nullPtr fd size

wlShmPoolCreateBuffer :: Ptr ShmPool -> CInt -> CInt -> CInt -> CInt -> CUInt -> IO (Ptr Buffer)
wlShmPoolCreateBuffer pool off w h s f = do
    let proxy = castPtr pool
    version <- wlProxyGetVersion proxy
    castPtr <$> wlProxyMarshalFlagsCreateBuffer proxy 0 wlBufferInferface version 0 nullPtr off w h s f

wlSurfaceAttach :: Ptr Surface -> Ptr Buffer -> CInt -> CInt -> IO ()
wlSurfaceAttach s b x y = do
    let proxy = castPtr s
    version <- wlProxyGetVersion proxy
    wlProxyMarshalFlagsBufferAttach proxy 1 nullPtr version 0 b x y >> pure ()

-- Call to make function pointers that can be used as callbacks into haskell defined function
foreign import ccall "wrapper" registryListenerGlobalWrapper :: (GlobalFn) -> IO (FunPtr GlobalFn)
foreign import ccall "wrapper" registryListenerGlobalRemoveWrapper :: (GlobalRemoveFn) -> IO (FunPtr GlobalRemoveFn)
foreign import ccall "wrapper" seatListenerCapabilitiesWrapper :: CapabilitesFn -> IO (FunPtr CapabilitesFn)
foreign import ccall "wrapper" seatListenerNameWrapper :: SlNameFn -> IO (FunPtr SlNameFn)
foreign import ccall "wrapper" xdgWmBaseListenerPingWrapper :: PingFn -> IO (FunPtr PingFn)
foreign import ccall "wrapper" xdgSurfaceListenerConfigureWrapper :: ConfigureFn -> IO (FunPtr ConfigureFn)
foreign import ccall "wrapper" noopWrapper :: (IO ()) -> IO (FunPtr (IO ()) )
foreign import ccall "wrapper" xdgTopCloseFnWrapper :: TopCloseFn -> IO (FunPtr TopCloseFn)
