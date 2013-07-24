module Common (assert, datadir, getTestContext) where

import Data.Maybe
import Control.Monad

import Text.XkbCommon

assert :: Bool -> String -> IO ()
assert False str = ioError (userError str)
assert _ _ = return ()

datadir = "data/"

getTestContext :: IO Context
getTestContext = do
   ctx <- liftM fromJust $ newContext pureFlags
   appendIncludePath ctx datadir
   return ctx
