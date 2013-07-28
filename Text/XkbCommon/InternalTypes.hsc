{-# LANGUAGE CPP, EmptyDataDecls, GeneralizedNewtypeDeriving #-}
module Text.XkbCommon.InternalTypes
   ( Context, CContext, InternalContext, toContext, fromContext, withContext,
     ContextFlags(..), defaultFlags,
     pureFlags, contextNoDefaultIncs, contextNoEnvironment,

     Keymap, CKeymap, InternalKeymap, toKeymap, fromKeymap, withKeymap, RMLVO(..), noPrefs,

     KeymapState, CKeymapState, toKeymapState, fromKeymapState, withKeymapState,

     readCString,

     Direction(..), keyUp, keyDown,

     CKeysym(..), Keysym(..), toKeysym, fromKeysym, safeToKeysym,

     CLogLevel(..), CKeycode(..), CLayoutIndex(..), CModIndex(..), CLevelIndex(..),
     CLedIndex(..), StateComponent(..), CModMask(..),
   ) where

import Foreign
import Foreign.C
import Foreign.Storable
import Control.Monad (ap)
import qualified Foreign.Storable.Newtype as Store
import Data.Flags

#include <xkbcommon/xkbcommon.h>

-- Context is the exposed datatype of an xkbcommon context
data Context = Context InternalContext
-- internal datatype and conversion methods...
data CContext
type InternalContext = ForeignPtr CContext
toContext :: InternalContext -> Context
toContext = Context
fromContext :: Context -> InternalContext
fromContext (Context ic) = ic
withContext :: Context -> (Ptr CContext -> IO a) -> IO a
withContext = withForeignPtr . fromContext

-- Keymap is struct xkb_keymap *
data Keymap = Keymap InternalKeymap
-- internals:
data CKeymap
type InternalKeymap = ForeignPtr CKeymap
toKeymap :: InternalKeymap -> Keymap
toKeymap = Keymap
fromKeymap :: Keymap -> InternalKeymap
fromKeymap (Keymap km) = km
withKeymap :: Keymap -> (Ptr CKeymap -> IO a) -> IO a
withKeymap = withForeignPtr . fromKeymap

{- the RMLVO type specifies preferences for keymap creation
   haskell equivalent of xkb_rule_names -}
data RMLVO = RMLVO {rules, model, layout, variant, options :: Maybe String}
noPrefs = RMLVO { rules = Nothing
                , model = Nothing
                , layout = Nothing
                , variant = Nothing
                , options = Nothing
                }

wrapCString :: CString -> IO (Maybe String)
wrapCString x = if x == nullPtr
   then return Nothing
   else do
      k <- peekCString x
      return $ Just k
wrapString :: Maybe String -> IO CString
wrapString Nothing = return nullPtr
wrapString (Just str) = newCString str
instance Storable RMLVO where
   sizeOf _ = #{size struct xkb_rule_names}
   alignment _ = alignment (undefined :: CInt)

   poke p rmlvo = do
      wrapString (rules rmlvo) >>= #{poke struct xkb_rule_names, rules} p
      wrapString (model rmlvo) >>= #{poke struct xkb_rule_names, model} p
      wrapString (layout rmlvo) >>= #{poke struct xkb_rule_names, layout} p
      wrapString (variant rmlvo) >>= #{poke struct xkb_rule_names, variant} p
      wrapString (options rmlvo) >>= #{poke struct xkb_rule_names, options} p
   peek p = return RMLVO
      `ap` (#{peek struct xkb_rule_names, rules} p >>= wrapCString)
      `ap` (#{peek struct xkb_rule_names, model} p >>= wrapCString)
      `ap` (#{peek struct xkb_rule_names, layout} p >>= wrapCString)
      `ap` (#{peek struct xkb_rule_names, variant} p >>= wrapCString)
      `ap` (#{peek struct xkb_rule_names, options} p >>= wrapCString)


-- KeymapState is struct xkb_state *
data KeymapState = KeymapState InternalKeymapState
-- internals:
data CKeymapState
type InternalKeymapState = ForeignPtr CKeymapState
toKeymapState :: InternalKeymapState -> KeymapState
toKeymapState = KeymapState
fromKeymapState :: KeymapState -> InternalKeymapState
fromKeymapState (KeymapState st) = st
withKeymapState :: KeymapState -> (Ptr CKeymapState -> IO a) -> IO a
withKeymapState = withForeignPtr . fromKeymapState


-- useful functions

-- reads a C string obtained from the library and proceeds to free it
readCString :: CString -> IO String
readCString cstr = do
   str <- peekCString cstr
   free cstr
   return str

newtype CKeysym = CKeysym {unCKeysym :: #{type xkb_keysym_t}} deriving (Show, Eq)
instance Storable CKeysym where
   sizeOf = Store.sizeOf unCKeysym
   alignment = Store.alignment unCKeysym
   peek = Store.peek CKeysym
   poke = Store.poke unCKeysym
newtype Keysym = Keysym Int deriving (Show, Eq)
fromKeysym :: Keysym -> CKeysym
fromKeysym (Keysym k) = CKeysym (fromIntegral k)
toKeysym :: CKeysym -> Keysym
toKeysym (CKeysym 0) = error "Keysym must be nonzero!"
toKeysym (CKeysym k) = Keysym (fromIntegral k)
safeToKeysym :: CKeysym -> Maybe Keysym
safeToKeysym (CKeysym 0) = Nothing
safeToKeysym (CKeysym n) = Just (Keysym (fromIntegral n))

newtype CKeycode = CKeycode {unCKeycode :: #{type xkb_keycode_t}} deriving (Show, Eq)
instance Storable CKeycode where
   sizeOf = Store.sizeOf unCKeycode
   alignment = Store.alignment unCKeycode
   peek = Store.peek CKeycode
   poke = Store.poke unCKeycode
-- not sure if the below is useful... commented out until it is.
-- fromKeycode :: Keycode -> CKeycode
-- fromKeycode (Keycode k) = CKeycode k
-- toKeycode :: CKeycode -> Keycode
-- toKeycode (CKeycode 0) = error "Keycode must be nonzero!"
-- safeToKeycode :: CKeycode -> Maybe Keycode
-- safeToKeycode (CKeycode 0) = Nothing
-- safeToKeycode (CKeycode n) = Just (Keycode n)


newtype ContextFlags = ContextFlags #{type enum xkb_context_flags}
   deriving (Eq, Flags, BoundedFlags)
#{enum ContextFlags, ContextFlags
, contextNoEnvironment = XKB_CONTEXT_NO_ENVIRONMENT_NAMES
, contextNoDefaultIncs = XKB_CONTEXT_NO_DEFAULT_INCLUDES
   }
defaultFlags = noFlags :: ContextFlags
pureFlags = allFlags :: ContextFlags

-- newtype CCompileFlags = CCompileFlags #{type enum xkb_keymap_compile_flags} -- only one option, so disabled
newtype Direction = Direction #{type enum xkb_key_direction}
#{enum Direction, Direction, keyUp = XKB_KEY_UP, keyDown = XKB_KEY_DOWN}
-- newtype CKeymapFormat = CKeymapFormat #{type enum xkb_keymap_format} -- only one option, so disabled
-- newtype CKeysymFlags = CKeysymFlags #{type enum xkb_keysym_flags} -- only one option, so disabled
newtype CLayoutIndex = CLayoutIndex #{type xkb_layout_index_t}
newtype CLedIndex = CLedIndex #{type xkb_led_index_t}
newtype CLevelIndex = CLevelIndex #{type xkb_level_index_t}
newtype CLogLevel = CLogLevel #{type enum xkb_log_level}
newtype CModIndex = CModIndex #{type xkb_mod_index_t}
newtype CModMask = CModMask #{type xkb_mod_mask_t}
newtype StateComponent = StateComponent #{type enum xkb_state_component} -- ATTENTION this is a bitmask!
   deriving (Eq, Flags, BoundedFlags)
#{enum StateComponent, StateComponent
, stateModDepressed  = XKB_STATE_MODS_DEPRESSED
, stateModsLatched   = XKB_STATE_MODS_LATCHED
, stateModsLocked    = XKB_STATE_MODS_LOCKED
, stateModsEffective = XKB_STATE_MODS_EFFECTIVE
, stateLayoutDepressed = XKB_STATE_LAYOUT_DEPRESSED
, stateLayoutLatched   = XKB_STATE_LAYOUT_LATCHED
, stateLayoutLocked    = XKB_STATE_LAYOUT_LOCKED
, stateLayoutEffective = XKB_STATE_LAYOUT_EFFECTIVE
, stateLeds            = XKB_STATE_LEDS
   }

-- TODO keysym data types
