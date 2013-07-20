{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Text.XkbCommon.InternalTypes
	( Context, CContext, InternalContext, toContext, fromContext, withContext,
	  ContextFlags, noDefaultIncludes, noEnvironmentNames, defaultFlags,
	  pureFlags,

	  Keymap, CKeymap, InternalKeymap, toKeymap, fromKeymap, withKeymap, RMLVO(..),

	  KeymapState, CKeymapState, toKeymapState, fromKeymapState, withKeymapState,

	  CLogLevel(..), CKeycode(..), CLayoutIndex(..), CModIndex(..), CLevelIndex(..),
	  CLedIndex(..), CKeysym(..), CDirection(..), CStateComponent(..), CModMask(..),
	) where

import Foreign
import Foreign.C
import Foreign.Storable
import Control.Monad (ap)

#include <xkbcommon/xkbcommon.h>

-- Context is the exposed datatype of an xkbcommon context
data Context = Context InternalContext
-- internal datatype and conversion methods...
data CContext
type InternalContext = ForeignPtr CContext
toContext :: InternalContext -> Context
toContext ic = Context ic
fromContext :: Context -> InternalContext
fromContext (Context ic) = ic
withContext :: Context -> (Ptr CContext -> IO a) -> IO a
withContext = withForeignPtr . fromContext

-- Option flags for context creation
data ContextFlags = ContextFlags
	{ noDefaultIncludes :: Bool
	, noEnvironmentNames :: Bool
	} deriving (Eq, Show)
defaultFlags = ContextFlags { noDefaultIncludes = False, noEnvironmentNames = False }
pureFlags = ContextFlags { noDefaultIncludes = True, noEnvironmentNames = True }

-- Keymap is struct xkb_keymap *
data Keymap = Keymap InternalKeymap
-- internals:
data CKeymap
type InternalKeymap = ForeignPtr CKeymap
toKeymap :: InternalKeymap -> Keymap
toKeymap km = Keymap km
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
		wrapString (rules rmlvo) >>= (#{poke struct xkb_rule_names, rules} p)
		wrapString (model rmlvo) >>= (#{poke struct xkb_rule_names, model} p)
		wrapString (layout rmlvo) >>= (#{poke struct xkb_rule_names, layout} p)
		wrapString (variant rmlvo) >>= (#{poke struct xkb_rule_names, variant} p)
		wrapString (options rmlvo) >>= (#{poke struct xkb_rule_names, options} p)
	peek p = return RMLVO
		`ap` ((#{peek struct xkb_rule_names, rules} p) >>= wrapCString)
		`ap` ((#{peek struct xkb_rule_names, model} p) >>= wrapCString)
		`ap` ((#{peek struct xkb_rule_names, layout} p) >>= wrapCString)
		`ap` ((#{peek struct xkb_rule_names, variant} p) >>= wrapCString)
		`ap` ((#{peek struct xkb_rule_names, options} p) >>= wrapCString)


-- KeymapState is struct xkb_state *
data KeymapState = KeymapState InternalKeymapState
-- internals:
data CKeymapState
type InternalKeymapState = ForeignPtr CKeymapState
toKeymapState :: InternalKeymapState -> KeymapState
toKeymapState st = KeymapState st
fromKeymapState :: KeymapState -> InternalKeymapState
fromKeymapState (KeymapState st) = st
withKeymapState :: KeymapState -> (Ptr CKeymapState -> IO a) -> IO a
withKeymapState = withForeignPtr . fromKeymapState


-- newtype CCompileFlags = CCompileFlags #{type enum xkb_keymap_compile_flags} -- only one option, so disabled
newtype CDirection = CDirection #{type enum xkb_key_direction}
newtype CKeycode = CKeycode #{type xkb_keycode_t}
-- newtype CKeymapFormat = CKeymapFormat #{type enum xkb_keymap_format} -- only one option, so disabled
newtype CKeysym = CKeysym #{type xkb_keysym_t}
-- newtype CKeysymFlags = CKeysymFlags #{type enum xkb_keysym_flags} -- only one option, so disabled
newtype CLayoutIndex = CLayoutIndex #{type xkb_layout_index_t}
newtype CLedIndex = CLedIndex #{type xkb_led_index_t}
newtype CLevelIndex = CLevelIndex #{type xkb_level_index_t}
newtype CLogLevel = CLogLevel #{type enum xkb_log_level}
newtype CModIndex = CModIndex #{type xkb_mod_index_t}
newtype CModMask = CModMask #{type xkb_mod_mask_t}
newtype CStateComponent = CStateComponent #{type enum xkb_state_component} -- ATTENTION this is a bitmask!

-- TODO keysym data types
