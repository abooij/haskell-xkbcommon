{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Text.XkbCommon.KeymapState
	( KeymapState(..), newKeymapState, updateKeymapState, getOneKeySym,
	) where

import Foreign
import Foreign.C
import Foreign.Storable
import Data.IORef
import Data.Functor
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Maybe (MaybeT)

import Text.XkbCommon.InternalTypes

#include <xkbcommon/xkbcommon.h>


-- create keymap state from keymap
newKeymapState :: Keymap -> IO KeymapState
newKeymapState km = withKeymap km $
		\ ptr -> do
			k <- c_new_keymap_state ptr
			l <- newForeignPtr c_unref_keymap_state k
			return $ toKeymapState l

{- TODO MAKE PROPER
-- the Int type may be replaced by a big enum-ish type, or maybe we should just 'type' it
-- return value should def. get its own type -}
updateKeymapState :: KeymapState -> Int -> Int -> IO Int
updateKeymapState st key dir = withKeymapState st $
		\ ptr -> fromIntegral <$> c_update_key_state ptr (fromIntegral key) (fromIntegral dir)

getOneKeySym :: KeymapState -> Int -> IO Int
getOneKeySym st key = withKeymapState st $
		\ ptr -> fromIntegral <$> c_get_one_key_sym ptr (fromIntegral key)


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

