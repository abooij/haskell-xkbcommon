{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Text.XkbCommon.Context
   ( Context(..), ContextFlags(..), defaultFlags, pureFlags, newContext,
     appendIncludePath, numIncludePaths, clearIncludePath, appendDefaultIncludePath,
     includePathShow,
   ) where

import Foreign
import Foreign.C
import Control.Monad (liftM)

import Text.XkbCommon.InternalTypes

#include <xkbcommon/xkbcommon.h>



-- construct a new Xkb context
-- xkb_context_new can fail if the default include path does not exist
newContext :: ContextFlags -> IO (Maybe Context)
newContext c = do
   k <- c_new_context $ translateContextFlags c
   if k == nullPtr
      then return Nothing
      else do
         l <- newForeignPtr c_unref_context k
         return $ Just $ toContext l

clearIncludePath :: Context -> IO ()
clearIncludePath ctx = withContext ctx $ \ ptr -> c_clear_includes ptr

-- stateful handling of Xkb context search paths for keymaps
-- fails if the path does not exist
appendIncludePath :: Context -> String -> IO (Maybe ())
appendIncludePath c str = withCString str $
   \ cstr -> withContext c $
      \ ptr -> do
         err <- c_append_include_path_context ptr cstr
         return $ if err == 1
            then Just ()
            else Nothing

appendDefaultIncludePath :: Context -> IO (Maybe ())
appendDefaultIncludePath ctx = withContext ctx $ \ ptr -> do
   ret <- c_append_default_include ptr -- returns 0 on error
   return (if ret == 0 then Nothing else Just ())

numIncludePaths :: Context -> IO Int
numIncludePaths c = withContext c $ liftM fromIntegral . c_num_include_paths_context

-- Get a specific include path from the context's include path.
-- c_show_include_path :: Ptr CContext -> CUInt -> IO CString
includePathShow :: Context -> Int -> IO String
includePathShow ctx idx = withContext ctx $ \ ptr -> c_show_include_path ptr (fromIntegral idx) >>= peekCString

-- BORING TRANSLATION STUFF

-- ugh, this one is rather ugly, but i really don't know how else to tackle it.
type CContextFlags = #type int
#{enum CContextFlags,
   , noFlags       = 0
   , noEnvNamFlags = XKB_CONTEXT_NO_ENVIRONMENT_NAMES
   , noDefIncFlags = XKB_CONTEXT_NO_DEFAULT_INCLUDES
   , bothFlags     = XKB_CONTEXT_NO_DEFAULT_INCLUDES + XKB_CONTEXT_NO_ENVIRONMENT_NAMES
   }

translateContextFlags :: ContextFlags -> CContextFlags
translateContextFlags x = j + k where
   j = if noDefaultIncludes x then noDefIncFlags else 0
   k = if noEnvironmentNames x then noEnvNamFlags else 0





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

-- int    xkb_context::xkb_context_include_path_append_default (struct xkb_context *context)
--     Append the default include paths to the contexts include path.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_include_path_append_default"
   c_append_default_include :: Ptr CContext -> IO CInt

-- int    xkb_context::xkb_context_include_path_reset_defaults (struct xkb_context *context)
--     Reset the context's include path to the default.
--foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_include_path_reset_defaults"

-- void    xkb_context::xkb_context_include_path_clear (struct xkb_context *context)
--     Remove all entries from the context's include path.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_include_path_clear"
   c_clear_includes :: Ptr CContext -> IO ()

-- const char *    xkb_context::xkb_context_include_path_get (struct xkb_context *context, unsigned int index)
--     Get a specific include path from the context's include path.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_include_path_get"
   c_show_include_path :: Ptr CContext -> CUInt -> IO CString


-- The foreign calls below are not yet bound... not sure I want to at this stage.

-- logging related

-- void    xkb_context::xkb_context_set_log_level (struct xkb_context *context, enum xkb_log_level level)
--     Set the current logging level.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_set_log_level"
   c_set_log_level :: Ptr CContext -> CLogLevel -> IO ()

-- enum xkb_log_level    xkb_context::xkb_context_get_log_level (struct xkb_context *context)
--     Get the current logging level.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_get_log_level"
   c_get_log_level :: Ptr CContext -> IO CLogLevel

-- void    xkb_context::xkb_context_set_log_verbosity (struct xkb_context *context, int verbosity)
--     Sets the current logging verbosity.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_set_log_verbosity"
   c_set_log_verbosity :: Ptr CContext -> CInt -> IO ()

-- int    xkb_context::xkb_context_get_log_verbosity (struct xkb_context *context)
--     Get the current logging verbosity of the context.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_get_log_verbosity"
   c_get_log_verbosity :: Ptr CContext -> IO CInt

-- we have to manually translate this in C because the haskell FFI does not support va_list!
-- void    xkb_context::xkb_context_set_log_fn (struct xkb_context *context, void(*log_fn)(struct xkb_context *context, enum xkb_log_level level, const char *format, va_list args))
--     Set a custom function to handle logging messages.
-- foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_set_log_fn"
--    c_set_log_fun :: Ptr CContext -> FunPtr (Ptr CContext -> CLogLevel -> CString -> #{type va_list} -> IO ()) -> IO ()

