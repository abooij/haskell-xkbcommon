module Common (assert, datadir, getTestContext, KeyDirections(..), testKeySeq, ks) where

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

data KeyDirections = Down | Up | Both | Repeat deriving (Show, Eq)

testKeySeq :: Keymap -> [(CKeycode, KeyDirections, Keysym)] -> IO [()]
testKeySeq km tests = do
   st <- newKeyboardState km
   return =<< mapM (testOne st) (zip tests [1..]) where
      testOne st ((kc, dir, ks),n) = do
         syms <- getStateSyms st kc

         when (dir == Down || dir == Both) $ void (updateKeyboardStateKey st kc keyDown)
         when (dir == Up || dir == Both) $ void (updateKeyboardStateKey st kc keyUp)

         -- in this test, we always get exactly one keysym
         assert (length syms == 1) "did not get right amount of keysyms"

         assert (head syms == ks) ("did not get correct keysym " ++ show ks
                                   ++ " for keycode " ++ show kc
                                   ++ ", got " ++ show (head syms)
                                   ++ " in test " ++ show n)

         -- this probably doesn't do anything since if we came this far, head syms == ks
         assert (keysymName (head syms) == keysymName ks) ("keysym names differ: " ++ keysymName (head syms) ++ " and " ++ keysymName ks)
         putStrLn $ keysymName ks
         return ()


ks = fromJust.keysymFromName
