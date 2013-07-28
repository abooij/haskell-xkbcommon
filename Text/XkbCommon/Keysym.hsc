{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Text.XkbCommon.Keysym
   ( keysymFromName, keysymFromCaselessName, keysymName, keysymUtf8
   ) where

import Foreign
import Foreign.C
import Foreign.Marshal.Array (peekArray0)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString, pack)
import System.IO.Unsafe as S
import Control.Monad (liftM)

import Text.XkbCommon.InternalTypes

#include <xkbcommon/xkbcommon.h>

-- keysym related

keysymFromName :: String -> Maybe Keysym
keysymFromName str = S.unsafePerformIO $ withCString str $ \ cstr ->
   liftM safeToKeysym $ c_keysym_from_name cstr 0 -- 0 means search case sensitive. alternative is #{const XKB_KEYSYM_CASE_INSENSITIVE}

-- search case for string case insensitively
keysymFromCaselessName :: String -> Maybe Keysym
keysymFromCaselessName str = S.unsafePerformIO $ withCString str $ \ cstr ->
   liftM safeToKeysym $ c_keysym_from_name cstr #{const XKB_KEYSYM_CASE_INSENSITIVE}

-- get string representation of a keysym
keysymName :: Keysym -> String
keysymName ks = S.unsafePerformIO $ do
   -- build 64-byte buffer and pass
   let buflen = 64
   withCString (replicate buflen ' ') $ \ cstr -> do
      len <- c_keysym_name (fromKeysym ks) cstr (fromIntegral buflen)
      return =<< peekCString cstr

keysymUtf8 :: Keysym -> String
keysymUtf8 ks = S.unsafePerformIO $ do
   let buflen = 64
   withCString (replicate buflen ' ') $ \ cstr -> do
      len <- c_keysym_utf8_name (fromKeysym ks) cstr (fromIntegral buflen)
      bs <- charPtrToByteString0 cstr
      return $ unpack $ decodeUtf8 bs


-- unsafely marshal zero-terminated char* to ByteString
charPtrToByteString0 :: Ptr CChar -> IO ByteString
charPtrToByteString0 ptr = do
   array <- peekArray0 0 ptr
   return $ pack $ map fromIntegral array


-- int    xkb_keysym_get_name (xkb_keysym_t keysym, char *buffer, size_t size)
--     Get the name of a keysym.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keysym_get_name"
   c_keysym_name :: CKeysym -> CString -> #{type size_t} -> IO CInt

-- xkb_keysym_t    xkb_keysym_from_name (const char *name, enum xkb_keysym_flags flags)
--     Get a keysym from its name.
-- second argument is always XKB_KEYSYM_CASE_INSENSITIVE
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keysym_from_name"
   c_keysym_from_name :: CString -> CInt -> IO CKeysym

-- below functions aren't bound yet.

-- int    xkb_keysym_to_utf8 (xkb_keysym_t keysym, char *buffer, size_t size)
--     Get the Unicode/UTF-8 representation of a keysym.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keysym_to_utf8"
   c_keysym_utf8_name :: CKeysym -> CString -> #{type size_t} -> IO CInt

-- uint32_t    xkb_keysym_to_utf32 (xkb_keysym_t keysym)
--     Get the Unicode/UTF-32 representation of a keysym.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keysym_to_utf32"
   c_keysym_utf32_name :: CKeysym -> #{type uint32_t}

