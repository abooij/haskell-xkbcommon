{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Text.XkbCommon.Keymap
   ( Keymap, RMLVO(..), noPrefs,

     newKeymapFromNames, newKeymapFromString, keymapAsString, keymapNumLayouts,
     keymapKeyNumLayouts, keymapNumMods, keymapModName, keymapModIdx, keymapNumLevels,
     keymapNumLeds,
     keymapLedName, keymapKeyRepeats
   ) where

import Foreign
import Foreign.C
import qualified System.IO.Unsafe as S (unsafePerformIO)

import Text.XkbCommon.InternalTypes

#include <xkbcommon/xkbcommon.h>


-- | Create keymap from optional preference of Rules+Model+Layouts+Variants+Options
--   'Keymap's are immutable but creation can fail. IO because it loads from disk.
--
--   (@xkb_keymap_new_from_names@)
newKeymapFromNames :: Context -> RMLVO -> IO (Maybe Keymap)
newKeymapFromNames ctx rmlvo = withContext ctx $ \ ptr -> do
   crmlvo <- new rmlvo
   k <- c_keymap_from_names ptr crmlvo #{const XKB_MAP_COMPILE_PLACEHOLDER }
   l <- newForeignPtr c_unref_keymap k
   return (if k == nullPtr then Nothing else Just $ toKeymap l)

-- | Create keymap from string buffer instead of loading from disk
--   Immutable but creation can fail. not IO because it just parses a string.
--
--   haskell-xkbcommon has no equivalent for xkb_keymap_new_from_file: just load it from disk
--   manually.
--
--   NOTE this can actually be an IO operation when compilation fails! (error output to stdout)
--
--   (@xkb_keymap_new_from_string@)
newKeymapFromString :: Context -> String -> Maybe Keymap
newKeymapFromString ctx buf = S.unsafePerformIO $ withCString buf $ \ cstr -> withContext ctx $ \ ptr -> do
   k <- c_keymap_from_string ptr cstr #{const XKB_KEYMAP_FORMAT_TEXT_V1} #{const XKB_MAP_COMPILE_PLACEHOLDER }
   l <- newForeignPtr c_unref_keymap k
   return (if k == nullPtr then Nothing else Just $ toKeymap l)

-- | Convert a keymap to an enormous string buffer. Opposite of 'newKeymapFromString'
--
--   (@xkb_keymap_get_as_string@)
keymapAsString :: Keymap -> String
keymapAsString km = S.unsafePerformIO $ withKeymap km $ \ ptr ->
   c_keymap_as_string ptr #{const XKB_KEYMAP_FORMAT_TEXT_V1} >>= peekCString

-- | Get the number of layouts in the keymap. (@xkb_keymap_num_layouts@)
keymapNumLayouts :: Keymap -> CLayoutIndex
keymapNumLayouts km = S.unsafePerformIO $ withKeymap km c_keymap_num_layouts

-- | Get the number of layouts for a specific key. (@xkb_keymap_num_layouts_for_key@)
keymapKeyNumLayouts :: Keymap -> CKeycode -> CLayoutIndex
keymapKeyNumLayouts km key = S.unsafePerformIO $ withKeymap km $ \ ptr -> c_keymap_num_layouts_key ptr key

-- | Get the name of a layout by index. (@xkb_keymap_layout_get_name@)
keymapLayoutName :: Keymap -> CLayoutIndex -> String
keymapLayoutName km idx = S.unsafePerformIO $ withKeymap km $
      \ ptr -> c_keymap_layout_name ptr idx >>= peekCString

-- | Get the number of modifiers in the keymap. (@xkb_keymap_num_mods@)
keymapNumMods :: Keymap -> CModIndex
keymapNumMods km = S.unsafePerformIO $ withKeymap km c_keymap_num_mods

-- | Get the name of a modifier by index. (@xkb_keymap_mod_get_name@)
keymapModName :: Keymap -> CModIndex -> String
keymapModName km idx = S.unsafePerformIO $ withKeymap km $
   \ ptr -> c_keymap_mod_name ptr idx >>= peekCString

-- | Get the index of a modifier by name. (@xkb_keymap_mod_get_index@)
keymapModIdx :: Keymap -> String -> Maybe CModIndex
keymapModIdx km name = S.unsafePerformIO $ withKeymap km $
   \ ptr -> withCString name $ \ cstr -> do
      idx <- c_keymap_mod_index ptr cstr
      case idx of
         CModIndex (#{const XKB_MOD_INVALID}) -> return Nothing
         x@(CModIndex n) -> return $ Just x

-- | Get the number of shift levels for a specific key and layout. (@xkb_keymap_num_levels_for_key@)
keymapNumLevels :: Keymap -> CKeycode -> CLayoutIndex -> CLevelIndex
keymapNumLevels km kc idx = S.unsafePerformIO $ withKeymap km $ \ ptr -> c_keymap_num_levels ptr kc idx

-- Get the keysyms obtained from pressing a key in a given layout and shift level.
-- c_keymap_syms_by_level :: Ptr CKeymap -> CKeycode -> CLayoutIndex -> CLevelIndex -> Ptr (Ptr CKeysym) -> IO CInt
-- TODO

-- | Get the number of LEDs in the keymap. (@xkb_keymap_num_leds@)
keymapNumLeds :: Keymap -> CLedIndex
keymapNumLeds km = S.unsafePerformIO $ withKeymap km c_keymap_num_leds

-- | Get the name of a LED by index. (@xkb_keymap_led_get_name@)
keymapLedName :: Keymap -> CLedIndex -> String
keymapLedName km id = S.unsafePerformIO . withKeymap km $
   \ ptr -> c_keymap_led_name ptr id >>= peekCString

-- | Determine whether a key should repeat or not. (@xkb_keymap_key_repeats@)
keymapKeyRepeats :: Keymap -> CKeycode -> Bool
keymapKeyRepeats km kc = S.unsafePerformIO (withKeymap km $ \ ptr -> c_keymap_key_repeats ptr kc) /= 0

-- FOREIGN CCALLS

-- struct xkb_keymap *    xkb_keymap::xkb_keymap_new_from_names (struct xkb_context *context, const struct xkb_rule_names *names, enum xkb_keymap_compile_flags flags)
-- note that we always pass 0 as the third argument since there are no options yet.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_new_from_names"
   c_keymap_from_names :: Ptr CContext -> Ptr RMLVO -> CInt -> IO (Ptr CKeymap)

-- struct xkb_keymap *    xkb_keymap::xkb_keymap_new_from_string (struct xkb_context *context, const char *string, enum xkb_keymap_format format, enum xkb_keymap_compile_flags flags)
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
   c_keymap_layout_name :: Ptr CKeymap -> CLayoutIndex -> IO CString

--xkb_mod_index_t    xkb_keymap::xkb_keymap_num_mods (struct xkb_keymap *keymap)
--    Get the number of modifiers in the keymap.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_num_mods"
   c_keymap_num_mods :: Ptr CKeymap -> IO CModIndex

-- const char *    xkb_keymap::xkb_keymap_mod_get_name (struct xkb_keymap *keymap, xkb_mod_index_t idx)
--     Get the name of a modifier by index.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_mod_get_name"
   c_keymap_mod_name :: Ptr CKeymap -> CModIndex -> IO CString

-- xkb_mod_index_t 	xkb_keymap::xkb_keymap_mod_get_index (struct xkb_keymap *keymap, const char *name)
--  	Get the index of a modifier by name. More...
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_mod_get_index"
   c_keymap_mod_index :: Ptr CKeymap -> CString -> IO CModIndex

-- xkb_layout_index_t    xkb_keymap::xkb_keymap_num_layouts (struct xkb_keymap *keymap)
--     Get the number of layouts in the keymap.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_num_layouts"
   c_keymap_num_layouts :: Ptr CKeymap -> IO CLayoutIndex

-- xkb_layout_index_t    xkb_keymap::xkb_keymap_num_layouts_for_key (struct xkb_keymap *keymap, xkb_keycode_t key)
--     Get the number of layouts for a specific key.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_num_layouts_for_key"
   c_keymap_num_layouts_key :: Ptr CKeymap -> CKeycode -> IO CLayoutIndex

-- xkb_level_index_t    xkb_keymap::xkb_keymap_num_levels_for_key (struct xkb_keymap *keymap, xkb_keycode_t key, xkb_layout_index_t layout)
--     Get the number of shift levels for a specific key and layout.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_num_levels_for_key"
   c_keymap_num_levels :: Ptr CKeymap -> CKeycode -> CLayoutIndex -> IO CLevelIndex

-- int    xkb_keymap::xkb_keymap_key_get_syms_by_level (struct xkb_keymap *keymap, xkb_keycode_t key, xkb_layout_index_t layout, xkb_level_index_t level, const xkb_keysym_t **syms_out)
--     Get the keysyms obtained from pressing a key in a given layout and shift level.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_key_get_syms_by_level"
   c_keymap_syms_by_level :: Ptr CKeymap -> CKeycode -> CLayoutIndex -> CLevelIndex -> Ptr (Ptr CKeysym) -> IO CInt

-- xkb_led_index_t    xkb_keymap::xkb_keymap_num_leds (struct xkb_keymap *keymap)
--     Get the number of LEDs in the keymap. More...
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_num_leds"
   c_keymap_num_leds :: Ptr CKeymap -> IO CLedIndex

-- const char *    xkb_keymap::xkb_keymap_led_get_name (struct xkb_keymap *keymap, xkb_led_index_t idx)
--     Get the name of a LED by index.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_led_get_name"
   c_keymap_led_name :: Ptr CKeymap -> CLedIndex -> IO CString

-- int    xkb_keymap::xkb_keymap_key_repeats (struct xkb_keymap *keymap, xkb_keycode_t key)
--     Determine whether a key should repeat or not.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_key_repeats"
   c_keymap_key_repeats :: Ptr CKeymap -> CKeycode -> IO CInt

