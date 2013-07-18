{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Text.XkbCommon
	( Context(..), ContextFlags, defaultFlags, newContext,
	  appendIncludePath, numIncludePaths,

	  Keymap(..), RMLVO, newKeymapFromNames, newKeymapFromString, keymapAsString,

	  KeymapState(..), newKeymapState, updateKeymapState, getOneKeySym,
	) where

import Foreign
import Foreign.C
import Foreign.Storable
import Data.IORef
import Data.Functor
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Maybe (MaybeT)

#include <xkbcommon/xkbcommon.h>





-- DATA TYPES

-- Context is the exposed datatype of an xkbcommon context
data Context = Context InternalContext
-- internal datatype and conversion methods...
data CContext
type InternalContext = ForeignPtr CContext
toContext :: InternalContext -> Context
toContext ic = Context ic
fromContext :: Context -> InternalContext
fromContext (Context ic) = ic


-- ditto for Keymap
data Keymap = Keymap InternalKeymap
-- internals:
data CKeymap
type InternalKeymap = ForeignPtr CKeymap
toKeymap :: InternalKeymap -> Keymap
toKeymap km = Keymap km
fromKeymap :: Keymap -> InternalKeymap
fromKeymap (Keymap km) = km

-- ditto for KeymapState
data KeymapState = KeymapState InternalKeymapState
-- internals:
data CKeymapState
type InternalKeymapState = ForeignPtr CKeymapState
toKeymapState :: InternalKeymapState -> KeymapState
toKeymapState st = KeymapState st
fromKeymapState :: KeymapState -> InternalKeymapState
fromKeymapState (KeymapState st) = st


-- Option flags for context creation
data ContextFlags = ContextFlags
	{ noDefaultIncludes :: Bool
	, noEnvironmentNames :: Bool
	} deriving (Eq, Show)
defaultFlags = ContextFlags { noDefaultIncludes = False, noEnvironmentNames = False }
pureFlags = ContextFlags { noDefaultIncludes = True, noEnvironmentNames = True }


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





-- FFI FUNCTIONS,   this is the important stuff


-- construct a new Xkb context
newContext :: ContextFlags -> IO (Maybe Context)
newContext c = do
	k <- c_new_context $ translateContextFlags c
	if k == nullPtr
		then return Nothing
		else do
			l <- newForeignPtr c_unref_context k
			return $ Just $ toContext l

-- stateful handling of Xkb context search paths for keymaps
appendIncludePath :: Context -> String -> IO (Maybe ())
appendIncludePath c str =
	withCString str $
		\ cstr -> withForeignPtr (fromContext c) $
			\ ptr -> do
				err <- c_append_include_path_context ptr cstr
				return $
					if err == 1
						then Just ()
						else Nothing


numIncludePaths :: Context -> IO Int
numIncludePaths c =
	let fptr = fromContext c
	in withForeignPtr fptr $
		\ ptr -> (liftM fromIntegral) $ c_num_include_paths_context ptr


-- create keymap from optional preference of Rules+Model+Layouts+Variants+Options
newKeymapFromNames :: Context -> RMLVO -> IO (Maybe Keymap)
newKeymapFromNames ctx rmlvo = withForeignPtr (fromContext ctx) $ \ ptr -> do
	crmlvo <- new rmlvo
	k <- c_keymap_from_names ptr crmlvo #{const XKB_MAP_COMPILE_PLACEHOLDER }
	l <- newForeignPtr c_unref_keymap k
	if k == nullPtr
		then return Nothing
		else return $ Just $ toKeymap l

-- create keymap from string buffer instead of loading from disk
newKeymapFromString :: Context -> String -> IO (Maybe Keymap)
newKeymapFromString ctx buf = withForeignPtr (fromContext ctx) $ \ ptr -> withCString buf $ \ cstr -> do
	k <- c_keymap_from_string ptr cstr #{const XKB_KEYMAP_FORMAT_TEXT_V1} #{const XKB_MAP_COMPILE_PLACEHOLDER }
	l <- newForeignPtr c_unref_keymap k
	if k == nullPtr
		then return Nothing
		else return . Just $ toKeymap l

-- convert a keymap to an enormous string buffer
keymapAsString :: Keymap -> IO String
keymapAsString km = withForeignPtr (fromKeymap km) $ \ ptr ->
	c_keymap_as_string ptr #{const XKB_KEYMAP_FORMAT_TEXT_V1} >>= peekCString

-- create keymap state from keymap
newKeymapState :: Keymap -> IO (Maybe KeymapState)
newKeymapState km =
	let fptr = fromKeymap km
	in withForeignPtr fptr $
		\ ptr -> do
			k <- c_new_keymap_state ptr
			l <- newForeignPtr c_unref_keymap_state k
			if k == nullPtr
				then return Nothing
				else return . Just $ toKeymapState l

-- c_keymap_layout_name :: Ptr CKeymap -> CInt -> IO CString
keymapLayoutName :: Keymap -> IO String
keymapLayoutName km =
	let fptr = fromKeymap km
	in withForeignPtr fptr $
		\ ptr -> c_keymap_layout_name ptr 0 >>= peekCString



{- TODO MAKE PROPER
-- the Int type may be replaced by a big enum-ish type, or maybe we should just 'type' it
-- return value should def. get its own type -}
updateKeymapState :: KeymapState -> Int -> Int -> IO Int
updateKeymapState st key dir =
	let fptr = fromKeymapState st
	in withForeignPtr fptr $
		\ ptr -> fromIntegral <$> c_update_key_state ptr (fromIntegral key) (fromIntegral dir)

getOneKeySym :: KeymapState -> Int -> IO Int
getOneKeySym st key =
	let fptr = fromKeymapState st
	in withForeignPtr fptr $
		\ ptr -> fromIntegral <$> c_get_one_key_sym ptr (fromIntegral key)





-- BORING TRANSLATION STUFF

type CContextFlags = #type int
#{enum CContextFlags,
	, noFlags       = 0
	, noEnvNamFlags = XKB_CONTEXT_NO_ENVIRONMENT_NAMES
	, noDefIncFlags = XKB_CONTEXT_NO_DEFAULT_INCLUDES
	, bothFlags     = XKB_CONTEXT_NO_DEFAULT_INCLUDES + XKB_CONTEXT_NO_ENVIRONMENT_NAMES
	}

translateContextFlags :: ContextFlags -> CContextFlags
translateContextFlags x = j + k where
	j = if noDefaultIncludes x then noEnvNamFlags else 0
	k = if noEnvironmentNames x then noDefIncFlags else 0





-- FOREIGN CCALLS


-- context related

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_new"
	c_new_context :: CContextFlags -> IO (Ptr CContext)

foreign import ccall unsafe "xkbcommon/xkbcommon.h &xkb_context_unref"
	c_unref_context :: FinalizerPtr CContext

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_include_path_append"
	c_append_include_path_context :: Ptr CContext -> CString -> IO CInt

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_num_include_paths"
	c_num_include_paths_context :: Ptr CContext -> IO CUInt

{-
int 	xkb_context::xkb_context_include_path_append_default (struct xkb_context *context)
 	Append the default include paths to the contexts include path.

int 	xkb_context::xkb_context_include_path_reset_defaults (struct xkb_context *context)
 	Reset the context's include path to the default.

void 	xkb_context::xkb_context_include_path_clear (struct xkb_context *context)
 	Remove all entries from the context's include path.

unsigned int 	xkb_context::xkb_context_num_include_paths (struct xkb_context *context)
 	Get the number of paths in the context's include path.

const char * 	xkb_context::xkb_context_include_path_get (struct xkb_context *context, unsigned int index)
 	Get a specific include path from the context's include path.
-}

-- keymap related

-- note that we always pass 0 as the third argument since there are no options yet.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_new_from_names"
	c_keymap_from_names :: Ptr CContext -> Ptr RMLVO -> CInt -> IO (Ptr CKeymap)

-- note that the third argument is always 1 because there are no options yet.
-- fourth is always 0
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_new_from_string"
	c_keymap_from_string :: Ptr CContext -> CString -> CInt -> CInt -> IO (Ptr CKeymap)

-- second argument 0 for V1, -1 for original (ie. V1).
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_get_as_string"
	c_keymap_as_string :: Ptr CKeymap -> CInt -> IO CString

foreign import ccall unsafe "xkbcommon/xkbcommon.h &xkb_keymap_unref"
	c_unref_keymap :: FinalizerPtr CKeymap

-- Get the name of a layout by index.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_layout_get_name"
	c_keymap_layout_name :: Ptr CKeymap -> CInt -> IO CString

{-
xkb_mod_index_t 	xkb_keymap::xkb_keymap_num_mods (struct xkb_keymap *keymap)
 	Get the number of modifiers in the keymap.

const char * 	xkb_keymap::xkb_keymap_mod_get_name (struct xkb_keymap *keymap, xkb_mod_index_t idx)
 	Get the name of a modifier by index.

xkb_mod_index_t 	xkb_keymap::xkb_keymap_mod_get_index (struct xkb_keymap *keymap, const char *name)
 	Get the index of a modifier by name.

xkb_layout_index_t 	xkb_keymap::xkb_keymap_num_layouts (struct xkb_keymap *keymap)
 	Get the number of layouts in the keymap.

xkb_layout_index_t 	xkb_keymap::xkb_keymap_layout_get_index (struct xkb_keymap *keymap, const char *name)
 	Get the index of a layout by name.

xkb_layout_index_t 	xkb_keymap::xkb_keymap_num_layouts_for_key (struct xkb_keymap *keymap, xkb_keycode_t key)
 	Get the number of layouts for a specific key.

xkb_level_index_t 	xkb_keymap::xkb_keymap_num_levels_for_key (struct xkb_keymap *keymap, xkb_keycode_t key, xkb_layout_index_t layout)
 	Get the number of shift levels for a specific key and layout.

int 	xkb_keymap::xkb_keymap_key_get_syms_by_level (struct xkb_keymap *keymap, xkb_keycode_t key, xkb_layout_index_t layout, xkb_level_index_t level, const xkb_keysym_t **syms_out)
 	Get the keysyms obtained from pressing a key in a given layout and shift level.

xkb_led_index_t 	xkb_keymap::xkb_keymap_num_leds (struct xkb_keymap *keymap)
 	Get the number of LEDs in the keymap. More...

const char * 	xkb_keymap::xkb_keymap_led_get_name (struct xkb_keymap *keymap, xkb_led_index_t idx)
 	Get the name of a LED by index.

xkb_led_index_t 	xkb_keymap::xkb_keymap_led_get_index (struct xkb_keymap *keymap, const char *name)
 	Get the index of a LED by name.

int 	xkb_keymap::xkb_keymap_key_repeats (struct xkb_keymap *keymap, xkb_keycode_t key)
 	Determine whether a key should repeat or not.
-}


-- keymap state related

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_new"
	c_new_keymap_state :: Ptr CKeymap -> IO (Ptr CKeymapState)

foreign import ccall unsafe "xkbcommon/xkbcommon.h &xkb_state_unref"
	c_unref_keymap_state :: FinalizerPtr CKeymapState

-- Below functions are not marshalled properly yet!!!

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_update_key"
	c_update_key_state :: Ptr CKeymapState -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_key_get_one_sym"
	c_get_one_key_sym :: Ptr CKeymapState -> CInt -> IO CInt

{-
int 	xkb_state::xkb_state_key_get_syms (struct xkb_state *state, xkb_keycode_t key, const xkb_keysym_t **syms_out)
 	Get the keysyms obtained from pressing a particular key in a given keyboard state.

xkb_layout_index_t 	xkb_state::xkb_state_key_get_layout (struct xkb_state *state, xkb_keycode_t key)
 	Get the effective layout index for a key in a given keyboard state.

xkb_level_index_t 	xkb_state::xkb_state_key_get_level (struct xkb_state *state, xkb_keycode_t key, xkb_layout_index_t layout)
 	Get the effective shift level for a key in a given keyboard state and layout.

enum xkb_state_component 	xkb_state::xkb_state_update_mask (struct xkb_state *state, xkb_mod_mask_t depressed_mods, xkb_mod_mask_t latched_mods, xkb_mod_mask_t locked_mods, xkb_layout_index_t depressed_layout, xkb_layout_index_t latched_layout, xkb_layout_index_t locked_layout)
 	Update a keyboard state from a set of explicit masks.

xkb_mod_mask_t 	xkb_state::xkb_state_serialize_mods (struct xkb_state *state, enum xkb_state_component components)
 	The counterpart to xkb_state_update_mask for modifiers, to be used on the server side of serialization.

xkb_layout_index_t 	xkb_state::xkb_state_serialize_layout (struct xkb_state *state, enum xkb_state_component components)
 	The counterpart to xkb_state_update_mask for layouts, to be used on the server side of serialization.

int 	xkb_state::xkb_state_mod_name_is_active (struct xkb_state *state, const char *name, enum xkb_state_component type)
 	Test whether a modifier is active in a given keyboard state by name.

int 	xkb_state::xkb_state_mod_names_are_active (struct xkb_state *state, enum xkb_state_component type, enum xkb_state_match match,...)
 	Test whether a set of modifiers are active in a given keyboard state by name.

int 	xkb_state::xkb_state_mod_index_is_active (struct xkb_state *state, xkb_mod_index_t idx, enum xkb_state_component type)
 	Test whether a modifier is active in a given keyboard state by index.

int 	xkb_state::xkb_state_mod_indices_are_active (struct xkb_state *state, enum xkb_state_component type, enum xkb_state_match match,...)
 	Test whether a set of modifiers are active in a given keyboard state by index.

int 	xkb_state::xkb_state_mod_index_is_consumed (struct xkb_state *state, xkb_keycode_t key, xkb_mod_index_t idx)
 	Test whether a modifier is consumed by keyboard state translation for a key.

xkb_mod_mask_t 	xkb_state::xkb_state_mod_mask_remove_consumed (struct xkb_state *state, xkb_keycode_t key, xkb_mod_mask_t mask)
 	Remove consumed modifiers from a modifier mask for a key.

int 	xkb_state::xkb_state_layout_name_is_active (struct xkb_state *state, const char *name, enum xkb_state_component type)
 	Test whether a layout is active in a given keyboard state by name.

int 	xkb_state::xkb_state_layout_index_is_active (struct xkb_state *state, xkb_layout_index_t idx, enum xkb_state_component type)
 	Test whether a layout is active in a given keyboard state by index.

int 	xkb_state::xkb_state_led_name_is_active (struct xkb_state *state, const char *name)
 	Test whether a LED is active in a given keyboard state by name.

int 	xkb_state::xkb_state_led_index_is_active (struct xkb_state *state, xkb_led_index_t idx)
 	Test whether a LED is active in a given keyboard state by index.
-}

-- keysym related
{-
int 	xkb_keysym_get_name (xkb_keysym_t keysym, char *buffer, size_t size)
 	Get the name of a keysym.

xkb_keysym_t 	xkb_keysym_from_name (const char *name, enum xkb_keysym_flags flags)
 	Get a keysym from its name.

int 	xkb_keysym_to_utf8 (xkb_keysym_t keysym, char *buffer, size_t size)
 	Get the Unicode/UTF-8 representation of a keysym.

uint32_t 	xkb_keysym_to_utf32 (xkb_keysym_t keysym)
 	Get the Unicode/UTF-32 representation of a keysym.
-}


-- logging related
{-
void 	xkb_context::xkb_context_set_log_level (struct xkb_context *context, enum xkb_log_level level)
 	Set the current logging level.

enum xkb_log_level 	xkb_context::xkb_context_get_log_level (struct xkb_context *context)
 	Get the current logging level.

void 	xkb_context::xkb_context_set_log_verbosity (struct xkb_context *context, int verbosity)
 	Sets the current logging verbosity.

int 	xkb_context::xkb_context_get_log_verbosity (struct xkb_context *context)
 	Get the current logging verbosity of the context.

void 	xkb_context::xkb_context_set_log_fn (struct xkb_context *context, void(*log_fn)(struct xkb_context *context, enum xkb_log_level level, const char *format, va_list args))
 	Set a custom function to handle logging messages.
-}

