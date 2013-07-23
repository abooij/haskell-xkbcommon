import Control.Monad
import Data.Maybe
import Data.Functor

import Text.XkbCommon

import Common

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

testFile :: Context -> String -> Bool -> IO ()
testFile ctx path shouldwork = do
   str <- readFile (datadir++path)
   let keymap = newKeymapFromString ctx str
   assert (shouldwork `xor` isNothing keymap) "file load failure"

main = do
   ctx <- fromJust <$> newContext defaultFlags
   mapM_ (\ path -> testFile ctx path True) [
	  "keymaps/basic.xkb",
	  "keymaps/comprehensive-plus-geom.xkb",
	  "keymaps/no-types.xkb"]
   mapM_ (\ path -> testFile ctx path False) [
	  "keymaps/divide-by-zero.xkb",
	  "keymaps/bad.xkb"]
