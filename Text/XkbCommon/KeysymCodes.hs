{-# LANGUAGE TemplateHaskell #-}

module Text.XkbCommon.KeysymCodes where

import Text.XkbCommon.ParseDefines
import Language.Haskell.TH

-- TH magic from ParseDefines:
$(runIO genKeysyms >>= return)
