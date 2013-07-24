{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Text.XkbCommon.Keysym
   ( keysymFromName
   ) where

import Foreign
import Foreign.C
import System.IO.Unsafe as S

import Text.XkbCommon.InternalTypes

#include <xkbcommon/xkbcommon.h>

-- keysym related

keysymFromName :: String -> CKeysym
keysymFromName str = S.unsafePerformIO $ withCString str $ \ cstr ->
   c_keysym_from_name cstr #{const XKB_KEYSYM_CASE_INSENSITIVE}

{-
keysymName :: CKeysym -> String
keysymName = unsafePerformIO $ do
   -- build 64-byte buffer and pass
   withCString (repeat 64 ' ')
-}

-- below functions aren't bound yet.

-- int    xkb_keysym_get_name (xkb_keysym_t keysym, char *buffer, size_t size)
--     Get the name of a keysym.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keysym_get_name"
   c_keysym_name :: CKeysym -> CString -> #{type size_t} -> IO CInt

-- xkb_keysym_t    xkb_keysym_from_name (const char *name, enum xkb_keysym_flags flags)
--     Get a keysym from its name.
-- second argument is always XKB_KEYSYM_CASE_INSENSITIVE
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keysym_from_name"
   c_keysym_from_name :: CString -> CInt -> IO CKeysym

-- int    xkb_keysym_to_utf8 (xkb_keysym_t keysym, char *buffer, size_t size)
--     Get the Unicode/UTF-8 representation of a keysym.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keysym_to_utf8"
   c_keysym_utf8_name :: CKeysym -> CString -> #{type size_t} -> CInt

-- uint32_t    xkb_keysym_to_utf32 (xkb_keysym_t keysym)
--     Get the Unicode/UTF-32 representation of a keysym.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keysym_to_utf32"
   c_keysym_utf32_name :: CKeysym -> #{type uint32_t}

