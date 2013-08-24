{-# LANGUAGE TemplateHaskell #-}

module Text.XkbCommon.ModList where

import Language.Haskell.TH

import Text.XkbCommon.ParseDefines

-- TH magic from ParseDefines:
$(runIO genModnames >>= return)
