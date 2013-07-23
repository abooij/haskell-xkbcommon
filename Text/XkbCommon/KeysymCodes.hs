{-# LANGUAGE TemplateHaskell #-}

module Text.XkbCommon.KeysymCodes where

import Text.XkbCommon.ParseDefines
import Language.Haskell.TH

keysym=True

$(runIO genKeysyms >>= return)
