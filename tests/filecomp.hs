import Control.Monad
import Data.Maybe
import Data.Functor

import Text.XkbCommon

import Common

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && (not (x && y))

test_file :: Context -> String -> Bool -> IO ()
test_file ctx path shouldwork = do
   str <- readFile (datadir++path)
   let keymap = newKeymapFromString ctx str
   assert (shouldwork `xor` isNothing keymap) "file load failure"

main = do
   ctx <- fromJust <$> newContext defaultFlags
   mapM (\ path -> test_file ctx path True) [
	  "keymaps/basic.xkb",
	  "keymaps/comprehensive-plus-geom.xkb",
	  "keymaps/no-types.xkb"]
   mapM (\ path -> test_file ctx path False) [
	  "keymaps/divide-by-zero.xkb",
	  "keymaps/bad.xkb"]
