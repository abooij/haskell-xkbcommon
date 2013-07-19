{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Text.XkbCommon.Keysym
	(
	) where

import Foreign
import Foreign.C
import Foreign.Storable
import Data.IORef
import Data.Functor
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Maybe (MaybeT)

#include <xkbcommon/xkbcommon.h>

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

