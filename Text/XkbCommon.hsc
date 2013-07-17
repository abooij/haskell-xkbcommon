{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Text.XkbCommon
	( Context(..), ContextFlags, defaultFlags, newContext,
	  appendIncludePath, numIncludePaths, 
	  
	  Keymap(..), RMLVO, newKeymapFromNames,

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

--Context is the exposed datatype of an xkbcommon context
data Context = Context InternalContext
--internal datatype and conversion methods...
data CContext
type InternalContext = ForeignPtr CContext
toContext :: InternalContext -> Context
toContext ic = Context ic
fromContext :: Context -> InternalContext
fromContext (Context ic) = ic


--ditto for Keymap
data Keymap = Keymap InternalKeymap
--internals:
data CKeymap
type InternalKeymap = ForeignPtr CKeymap
toKeymap :: InternalKeymap -> Keymap
toKeymap km = Keymap km
fromKeymap :: Keymap -> InternalKeymap
fromKeymap (Keymap km) = km

--ditto for KeymapState
data KeymapState = KeymapState InternalKeymapState
--internals:
data CKeymapState
type InternalKeymapState = ForeignPtr CKeymapState
toKeymapState :: InternalKeymapState -> KeymapState
toKeymapState st = KeymapState st
fromKeymapState :: KeymapState -> InternalKeymapState
fromKeymapState (KeymapState st) = st


--Option flags for context creation
data ContextFlags = ContextFlags
	{ noDefaultIncludes :: Bool
	, noEnvironmentNames :: Bool
	}
defaultFlags = ContextFlags { noDefaultIncludes = False, noEnvironmentNames = False }
pureFlags = ContextFlags { noDefaultIncludes = True, noEnvironmentNames = True }


--the RMLVO type specifies preferences for keymap creation
--haskell equivalent of xkb_rule_names
data RMLVO = RMLVO {rules,model,layout,variant,options :: Maybe String}
noPrefs = RMLVO { rules = Nothing
                , model = Nothing
					 , layout = Nothing
					 , variant = Nothing
					 , options = Nothing
					 }

wrapCString :: CString -> IO (Maybe String)
wrapCString x = if x==nullPtr
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

	



-- FFI FUNCTIONS
-- this is the important stuff


--construct a new Xkb context
newContext :: ContextFlags -> IO Context
newContext c =
	let k = c_new_context $ translateContextFlags c
	in (liftM toContext) $ k >>= newForeignPtr c_unref_context


--stateful handling of Xkb context search paths for keymaps
appendIncludePath :: Context -> String -> IO (Maybe ())
appendIncludePath c str =
	withCString str $
		\cstr -> withForeignPtr (fromContext c) $
			\ptr -> do
				err <- c_append_include_path_context ptr cstr
				return $
					if err==1
						then Just ()
						else Nothing


numIncludePaths :: Context -> IO Int
numIncludePaths c =
	let fptr = fromContext c
	in withForeignPtr fptr $
		\ptr -> (liftM fromIntegral) $ c_num_include_paths_context ptr


-- create keymap from optional preference of Rules+Model+Layouts+Variants+Options
newKeymapFromNames :: Context -> RMLVO -> IO Keymap
newKeymapFromNames ctx rmlvo = withForeignPtr (fromContext ctx) $ \ptr -> do
	crmlvo <- new rmlvo
	k <- c_keymap_from_names ptr crmlvo 0
	l <- newForeignPtr c_unref_keymap k
	return $ toKeymap l

-- create keymap state from keymap
newKeymapState :: Keymap -> IO KeymapState
newKeymapState km =
	let fptr = fromKeymap km
	in withForeignPtr fptr $
		\ptr -> do
			k <- c_new_keymap_state ptr
			l <- newForeignPtr c_unref_keymap_state k
			return $ toKeymapState l




-- TODO MAKE PROPER
-- the Int type may be replaced by a big enum-ish type, or maybe we should just 'type' it
updateKeymapState :: KeymapState -> Int -> Int -> IO Int
updateKeymapState st key dir =
	let fptr = fromKeymapState st
	in withForeignPtr fptr $
		\ptr -> fromIntegral <$> c_update_key_state ptr (fromIntegral key) (fromIntegral dir)
			
getOneKeySym :: KeymapState -> Int -> IO Int
getOneKeySym st key =
	let fptr = fromKeymapState st
	in withForeignPtr fptr $
		\ptr -> fromIntegral <$> c_get_one_key_sym ptr (fromIntegral key)
			




-- BORING TRANSLATION STUFF

type CContextFlags = #type int
#{enum CContextFlags,
	, noFlags       = 0
	, noEnvNamFlags = XKB_CONTEXT_NO_ENVIRONMENT_NAMES
	, noDefIncFlags = XKB_CONTEXT_NO_DEFAULT_INCLUDES
	, bothFlags     = XKB_CONTEXT_NO_DEFAULT_INCLUDES+XKB_CONTEXT_NO_ENVIRONMENT_NAMES
	}

translateContextFlags :: ContextFlags -> CContextFlags
translateContextFlags x = j + k where
	j = if noDefaultIncludes x then noEnvNamFlags else 0
	k = if noEnvironmentNames x then noDefIncFlags else 0




-- FOREIGN CCALLS

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_new"
	c_new_context :: CContextFlags -> IO (Ptr CContext)

foreign import ccall unsafe "xkbcommon/xkbcommon.h &xkb_context_unref"
	c_unref_context :: FinalizerPtr CContext

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_include_path_append"
	c_append_include_path_context :: Ptr CContext -> CString -> IO CInt

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_context_num_include_paths"
	c_num_include_paths_context :: Ptr CContext -> IO CUInt

-- note that we always pass 0 as the third argument since there are no options yet.
foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_keymap_new_from_names"
	c_keymap_from_names :: Ptr CContext -> Ptr RMLVO -> CInt -> IO (Ptr CKeymap)

foreign import ccall unsafe "xkbcommon/xkbcommon.h &xkb_keymap_unref"
	c_unref_keymap :: FinalizerPtr CKeymap

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_new"
	c_new_keymap_state :: Ptr CKeymap -> IO (Ptr CKeymapState)

foreign import ccall unsafe "xkbcommon/xkbcommon.h &xkb_state_unref"
	c_unref_keymap_state :: FinalizerPtr CKeymapState

--Below functions are not marshalled properly yet!!!

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_update_key"
	c_update_key_state :: Ptr CKeymapState -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "xkbcommon/xkbcommon.h xkb_state_key_get_one_sym"
	c_get_one_key_sym :: Ptr CKeymapState -> CInt -> IO CInt
