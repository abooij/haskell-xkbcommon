{-# LANGUAGE CPP, EmptyDataDecls, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Text.XkbCommon.InternalTypes
   ( Context, CContext, InternalContext, toContext, fromContext, withContext,
     ContextFlags(..), defaultFlags,
     pureFlags, contextNoDefaultIncludes, contextNoEnvironment,

     Keymap, CKeymap, InternalKeymap, toKeymap, fromKeymap, withKeymap, RMLVO(..), noPrefs,

     KeyboardState, CKeyboardState, toKeyboardState, fromKeyboardState, withKeyboardState,

     readCString,

     Direction(..), keyUp, keyDown,

     CKeysym(..), Keysym(..), toKeysym, fromKeysym, safeToKeysym,

     CLogLevel(..), CKeycode(..), CLayoutIndex(..), CModIndex(..), CLevelIndex(..),
     CLedIndex(..), StateComponent(..), CModMask(..),

     stateModDepressed, stateModLatched, stateModLocked, stateModEffective,
     stateLayoutDepressed, stateLayoutLatched, stateLayoutLocked,
     stateLayoutEffective, stateLeds,
   ) where

import Foreign
import Foreign.C
import Foreign.Storable
import Control.Monad (ap, liftM)
import qualified Foreign.Storable.Newtype as Store
import Data.Flags
import Data.Flags.TH

#include <xkbcommon/xkbcommon.h>

-- | @Context@ is the exposed datatype of an xkbcommon context (@struct xkb_context*@)
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

-- | Keymap represents a compiled keymap object. (@struct xkb_keymap*@)
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

-- | The RMLVO type specifies preferences for keymap creation
--   (@struct xkb_rule_names*@)
data RMLVO = RMLVO {rules, model, layout, variant, options :: Maybe String}
-- | Specify that no specific keymap is preferred by the program.
--   Depending on the specified 'ContextFlags' during 'Context' creation,
--   'RMLVO' specifications may be loaded from environment variables.
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


-- | @KeyboardState@ represents the state of a connected keyboard. (@struct xkb_state*@)
data KeyboardState = KeyboardState InternalKeyboardState
-- internals:
data CKeyboardState
type InternalKeyboardState = ForeignPtr CKeyboardState
toKeyboardState :: InternalKeyboardState -> KeyboardState
toKeyboardState = KeyboardState
fromKeyboardState :: KeyboardState -> InternalKeyboardState
fromKeyboardState (KeyboardState st) = st
withKeyboardState :: KeyboardState -> (Ptr CKeyboardState -> IO a) -> IO a
withKeyboardState = withForeignPtr . fromKeyboardState


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
-- | One graphical symbol (usually on-screen). This is the end product of libxkbcommon.
--   Some keysyms are not graphical characters, but can also represent e.g. Left or Right arrow
--   keys. Refer to the libxkbcommon documentation for details.
--
--   NOTE that @XKB_KEY_NoSymbol@ is represented by a @Nothing@ in haskell-xkbcommon.
--
--   (@xkb_keysym_t@)
newtype Keysym = Keysym Int deriving (Show, Eq)
fromKeysym :: Keysym -> CKeysym
fromKeysym (Keysym k) = CKeysym (fromIntegral k)
toKeysym :: CKeysym -> Keysym
toKeysym (CKeysym 0) = error "Keysym must be nonzero!"
toKeysym (CKeysym k) = Keysym (fromIntegral k)
safeToKeysym :: CKeysym -> Maybe Keysym
safeToKeysym (CKeysym 0) = Nothing
safeToKeysym (CKeysym n) = Just (Keysym (fromIntegral n))

-- | One keyboard key. Events on keys are the input of libxkbcommon.
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

-- | @ContextFlags@ carry options for construction of a 'Context'. (@enum xkb_context_flags@)
newtype ContextFlags = ContextFlags #{type enum xkb_context_flags}
   deriving (Eq, Flags)

-- tail.init because the first item is the type declaration (which we can already find above) and the last is the Show instance (which we don't want/need)
$(liftM (tail.init) $ bitmaskWrapper "ContextFlags" ''#{type enum xkb_context_flags} []
   [("contextNoEnvironment", #{const XKB_CONTEXT_NO_ENVIRONMENT_NAMES}),
    ("contextNoDefaultIncludes", #{const XKB_CONTEXT_NO_DEFAULT_INCLUDES})])
-- | Default 'ContextFlags': consider RMLVO prefs from the environment variables, and search for 'Keymap' files in the default paths.
defaultFlags = noFlags :: ContextFlags
-- | Pure 'ContextFlags': don't consider env vars or default search paths, which are system-dependent.
pureFlags = contextNoEnvironment .+. contextNoDefaultIncludes

-- newtype CCompileFlags = CCompileFlags #{type enum xkb_keymap_compile_flags} -- only one option, so disabled
-- | In a key event, a key can be pressed\/moved down ('keyDown') or released\/moved up ('keyUp').
newtype Direction = Direction #{type enum xkb_key_direction}
#{enum Direction, Direction, keyUp = XKB_KEY_UP, keyDown = XKB_KEY_DOWN}
-- newtype CKeymapFormat = CKeymapFormat #{type enum xkb_keymap_format} -- only one option, so disabled
-- newtype CKeysymFlags = CKeysymFlags #{type enum xkb_keysym_flags} -- only one option, so disabled

-- | Index of a keyboard layout.
--
--   The layout index is a state component which detemines which keyboard layout is active.
--   These may be different alphabets, different key arrangements, etc.
--
--   Layout indexes are consecutive. The first layout has index 0.
--
--   Each layout is not required to have a name, and the names are not guaranteed to be unique
--   (though they are usually provided and unique).
--   Therefore, it is not safe to use the name as a unique identifier for a layout.
--   Layout names are case-sensitive.
--
--   Layouts are also called "groups" by XKB.
newtype CLayoutIndex = CLayoutIndex #{type xkb_layout_index_t}
newtype CLedIndex = CLedIndex #{type xkb_led_index_t}
-- | Index of a shift level.
newtype CLevelIndex = CLevelIndex #{type xkb_level_index_t}
newtype CLogLevel = CLogLevel #{type enum xkb_log_level}
-- | Index of a modifier.
--
--   A modifier is a state component which changes the way keys are interpreted.
--   A keymap defines a set of modifiers, such as Alt, Shift, Num Lock or Meta,
--   and specifies which keys may activate which modifiers (in a many-to-many relationship,
--   i.e. a key can activate several modifiers, and a modifier may be activated by several keys.
--   Different keymaps do this differently).
--
--   When retrieving the keysyms for a key, the active modifier set is consulted;
--   this detemines the correct shift level to use within the currently active layout
--   (see 'CLevelIndex').
--
--   Modifier indexes are consecutive. The first modifier has index 0.
newtype CModIndex = CModIndex {unCModIndex :: #{type xkb_mod_index_t}} deriving (Show, Eq)
instance Storable CModIndex where
   sizeOf = Store.sizeOf unCModIndex
   alignment = Store.alignment unCModIndex
   peek = Store.peek CModIndex
   poke = Store.poke unCModIndex
newtype CModMask = CModMask #{type xkb_mod_mask_t} deriving(Eq, Num, Show)
-- | Modifier and layout types for state objects.
--
--   In XKB, the DEPRESSED components are also known as \'base\'.
--
--   (@xkb_state_component@)
newtype StateComponent = StateComponent #{type enum xkb_state_component} -- ATTENTION this is a bitmask!
   deriving (Eq, Flags, BoundedFlags)
#{enum StateComponent, StateComponent
, stateModDepressed    = XKB_STATE_MODS_DEPRESSED
, stateModLatched      = XKB_STATE_MODS_LATCHED
, stateModLocked       = XKB_STATE_MODS_LOCKED
, stateModEffective    = XKB_STATE_MODS_EFFECTIVE
, stateLayoutDepressed = XKB_STATE_LAYOUT_DEPRESSED
, stateLayoutLatched   = XKB_STATE_LAYOUT_LATCHED
, stateLayoutLocked    = XKB_STATE_LAYOUT_LOCKED
, stateLayoutEffective = XKB_STATE_LAYOUT_EFFECTIVE
, stateLeds            = XKB_STATE_LEDS
   }

-- TODO keysym data types
