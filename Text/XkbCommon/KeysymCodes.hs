{-# LANGUAGE TemplateHaskell #-}

module Text.XkbCommon.KeysymCodes where

import Language.Haskell.TH

import Text.XkbCommon.ParseDefines
import Text.XkbCommon.InternalTypes

-- TH magic from ParseDefines:
$(runIO genKeysyms >>= return)
