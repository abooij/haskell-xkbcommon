{-# LANGUAGE TemplateHaskell #-}

module Text.XkbCommon.KeycodeList where

import Language.Haskell.TH

import Text.XkbCommon.ParseDefines
import Text.XkbCommon.InternalTypes

-- TH magic from ParseDefines:
$(runIO genKeycodes >>= return)

toEvdev :: CKeycode -> Int
toEvdev (CKeycode k) = fromIntegral k - 8
fromEvdev :: Int -> CKeycode
fromEvdev k = CKeycode $ fromIntegral (k + 8)
