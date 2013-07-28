{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Text.XkbCommon.KeymapState
   ( KeymapState, newKeymapState, updateKeymapState, getOneKeySym, getStateSyms
   ) where

import Foreign
import Foreign.C
import Foreign.Storable
import Data.Functor
import Data.Maybe (mapMaybe)

import Text.XkbCommon.InternalTypes

#include <xkbcommon/xkbcommon.h>


-- create keymap state from keymap
newKeymapState :: Keymap -> IO KeymapState
newKeymapState km = withKeymap km $
      \ ptr -> do
         k <- c_new_keymap_state ptr
         l <- newForeignPtr c_unref_keymap_state k
         return $ toKeymapState l

updateKeymapState :: KeymapState -> CKeycode -> Direction -> IO StateComponent
updateKeymapState st key dir = withKeymapState st $
      \ ptr -> c_update_key_state ptr key dir

getOneKeySym :: KeymapState -> CKeycode -> IO (Maybe Keysym)
getOneKeySym st key = withKeymapState st $
      \ ptr -> do
         ks <- c_get_one_key_sym ptr key
         return $ safeToKeysym ks

-- TODO TEST TEST TEST I HAVE NO IDEA IF THIS WORKS!!!
-- Get the keysyms obtained from pressing a particular key in a given keyboard state.
-- c_state_get_syms :: Ptr CKeymapState -> CKeycode -> Ptr (Ptr CKeysym) -> IO CInt
getStateSyms :: KeymapState -> CKeycode -> IO [Keysym]
getStateSyms ks key = withKeymapState ks $ \ ptr -> do
   init_ptr <- newArray [] :: IO (Ptr CKeysym)
   in_ptr <- new init_ptr
   num_out <- c_state_get_syms ptr key in_ptr
   deref_ptr <- peek in_ptr
   out_list <- peekArray (fromIntegral num_out) deref_ptr
   --free deref_ptr >> free in_ptr >> free init_ptr
   free in_ptr >> free init_ptr
   return $ mapMaybe safeToKeysym out_list

-- Get the effective layout index for a key in a given keyboard state.
-- c_get_layout :: Ptr CKeymapState -> CKeycode -> IO CLayoutIndex

-- Get the effective shift level for a key in a given keyboard state and layout.
-- c_key_get_level :: Ptr CKeymapState -> CKeycode -> CLayoutIndex -> IO CLevelIndex

-- Update a keyboard state from a set of explicit masks.
-- c_update_state_mask :: Ptr CKeymapState -> CModMask -> CModMask -> CModMask -> CLayoutIndex -> CLayoutIndex -> CLayoutIndex -> IO StateComponent

-- The counterpart to xkb_state_update_mask for modifiers, to be used on the server side of serialization.
-- c_serialize_state_mods :: Ptr CKeymapState -> StateComponent -> IO CModMask

-- The counterpart to xkb_state_update_mask for layouts, to be used on the server side of serialization.
-- c_serialize_state :: Ptr CKeymapState -> StateComponent -> IO CLayoutIndex

-- Test whether a modifier is active in a given keyboard state by name.
-- c_state_mod_name_is_active :: Ptr CKeymapState -> CString -> StateComponent -> IO Int

-- Test whether a modifier is active in a given keyboard state by index.
-- c_state_mod_idx_is_active :: Ptr CKeymapState -> CModIndex -> StateComponent -> IO CInt

-- Test whether a modifier is consumed by keyboard state translation for a key.
-- c_modifier_is_consumed :: Ptr CKeymapState -> CKeycode -> CModIndex -> IO CInt

-- Remove consumed modifiers from a modifier mask for a key.
-- c_remove_consumed_modifiers :: Ptr CKeymapState -> CKeycode -> CModMask -> IO CModMask

-- Test whether a layout is active in a given keyboard state by name.
-- c_layout_name_is_active :: Ptr CKeymapState -> CString -> StateComponent -> IO CInt

-- Test whether a layout is active in a given keyboard state by index.
-- c_layout_index_is_active :: Ptr CKeymapState -> CLayoutIndex -> StateComponent -> IO CInt

-- Test whether a LED is active in a given keyboard state by name.
-- c_led_name_is_active :: Ptr CKeymapState -> CString -> IO CInt

-- Test whether a LED is active in a given keyboard state by index.
-- c_led_index_is_active :: Ptr CKeymapState -> CLedIndex -> IO CInt



-- keymap state related

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_new"
   c_new_keymap_state :: Ptr CKeymap -> IO (Ptr CKeymapState)

foreign import ccall unsafe "xkbcommon/xkbcommon.h &xkb_state_unref"
   c_unref_keymap_state :: FinalizerPtr CKeymapState

-- Below functions are not marshalled properly yet!!!

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_update_key"
   c_update_key_state :: Ptr CKeymapState -> CKeycode -> Direction -> IO StateComponent

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_key_get_one_sym"
   c_get_one_key_sym :: Ptr CKeymapState -> CKeycode -> IO CKeysym



-- The below functions aren't bound yet.

-- int    xkb_state::xkb_state_key_get_syms (struct xkb_state *state, xkb_keycode_t key, const xkb_keysym_t **syms_out)
--     Get the keysyms obtained from pressing a particular key in a given keyboard state.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_key_get_syms"
   c_state_get_syms :: Ptr CKeymapState -> CKeycode -> Ptr (Ptr CKeysym) -> IO CInt

-- xkb_layout_index_t    xkb_state::xkb_state_key_get_layout (struct xkb_state *state, xkb_keycode_t key)
--     Get the effective layout index for a key in a given keyboard state.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_key_get_layout"
   c_get_layout :: Ptr CKeymapState -> CKeycode -> IO CLayoutIndex

-- xkb_level_index_t    xkb_state::xkb_state_key_get_level (struct xkb_state *state, xkb_keycode_t key, xkb_layout_index_t layout)
--     Get the effective shift level for a key in a given keyboard state and layout.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_key_get_level"
   c_key_get_level :: Ptr CKeymapState -> CKeycode -> CLayoutIndex -> IO CLevelIndex

-- enum xkb_state_component    xkb_state::xkb_state_update_mask (struct xkb_state *state, xkb_mod_mask_t depressed_mods, xkb_mod_mask_t latched_mods, xkb_mod_mask_t locked_mods, xkb_layout_index_t depressed_layout, xkb_layout_index_t latched_layout, xkb_layout_index_t locked_layout)
--     Update a keyboard state from a set of explicit masks.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_update_mask"
   c_update_state_mask :: Ptr CKeymapState -> CModMask -> CModMask -> CModMask -> CLayoutIndex -> CLayoutIndex -> CLayoutIndex -> IO StateComponent

-- xkb_mod_mask_t    xkb_state::xkb_state_serialize_mods (struct xkb_state *state, enum xkb_state_component components)
--     The counterpart to xkb_state_update_mask for modifiers, to be used on the server side of serialization.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_serialize_mods"
   c_serialize_state_mods :: Ptr CKeymapState -> StateComponent -> IO CModMask

-- xkb_layout_index_t    xkb_state::xkb_state_serialize_layout (struct xkb_state *state, enum xkb_state_component components)
--     The counterpart to xkb_state_update_mask for layouts, to be used on the server side of serialization.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_serialize_layout"
   c_serialize_state :: Ptr CKeymapState -> StateComponent -> IO CLayoutIndex

-- int    xkb_state::xkb_state_mod_name_is_active (struct xkb_state *state, const char *name, enum xkb_state_component type)
--     Test whether a modifier is active in a given keyboard state by name.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_mod_name_is_active"
   c_state_mod_name_is_active :: Ptr CKeymapState -> CString -> StateComponent -> IO Int

-- cannot be ccalled due to va_list
-- int    xkb_state::xkb_state_mod_names_are_active (struct xkb_state *state, enum xkb_state_component type, enum xkb_state_match match,...)
--     Test whether a set of modifiers are active in a given keyboard state by name.
-- foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_mod_names_are_active"

-- int    xkb_state::xkb_state_mod_index_is_active (struct xkb_state *state, xkb_mod_index_t idx, enum xkb_state_component type)
--     Test whether a modifier is active in a given keyboard state by index.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_mod_index_is_active"
   c_state_mod_idx_is_active :: Ptr CKeymapState -> CModIndex -> StateComponent -> IO CInt

-- cannot be ccalled due to va_list
-- int    xkb_state::xkb_state_mod_indices_are_active (struct xkb_state *state, enum xkb_state_component type, enum xkb_state_match match,...)
--     Test whether a set of modifiers are active in a given keyboard state by index.
-- foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_mod_indices_are_active"

-- int    xkb_state::xkb_state_mod_index_is_consumed (struct xkb_state *state, xkb_keycode_t key, xkb_mod_index_t idx)
--     Test whether a modifier is consumed by keyboard state translation for a key.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_mod_index_is_consumed"
   c_modifier_is_consumed :: Ptr CKeymapState -> CKeycode -> CModIndex -> IO CInt

-- xkb_mod_mask_t    xkb_state::xkb_state_mod_mask_remove_consumed (struct xkb_state *state, xkb_keycode_t key, xkb_mod_mask_t mask)
--     Remove consumed modifiers from a modifier mask for a key.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_mod_mask_remove_consumed"
   c_remove_consumed_modifiers :: Ptr CKeymapState -> CKeycode -> CModMask -> IO CModMask

-- int    xkb_state::xkb_state_layout_name_is_active (struct xkb_state *state, const char *name, enum xkb_state_component type)
--     Test whether a layout is active in a given keyboard state by name.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_layout_name_is_active"
   c_layout_name_is_active :: Ptr CKeymapState -> CString -> StateComponent -> IO CInt

-- int    xkb_state::xkb_state_layout_index_is_active (struct xkb_state *state, xkb_layout_index_t idx, enum xkb_state_component type)
--     Test whether a layout is active in a given keyboard state by index.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_layout_index_is_active"
   c_layout_index_is_active :: Ptr CKeymapState -> CLayoutIndex -> StateComponent -> IO CInt

-- int    xkb_state::xkb_state_led_name_is_active (struct xkb_state *state, const char *name)
--     Test whether a LED is active in a given keyboard state by name.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_led_name_is_active"
   c_led_name_is_active :: Ptr CKeymapState -> CString -> IO CInt

-- int    xkb_state::xkb_state_led_index_is_active (struct xkb_state *state, xkb_led_index_t idx)
--     Test whether a LED is active in a given keyboard state by index.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_led_index_is_active"
   c_led_index_is_active :: Ptr CKeymapState -> CLedIndex -> IO CInt

