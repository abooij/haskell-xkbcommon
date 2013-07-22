import Control.Monad
import Data.Maybe

import Text.XkbCommon

import Common

main = do
   ctx <- liftM fromJust $ newContext defaultFlags

   original <- readFile (datadir++"keymaps/stringcomp.data")

   let keymap = newKeymapFromString ctx original
   do case keymap of
		Nothing -> assert False "Could not read keymap string!"
		Just _ -> assert True undefined

   let dump = keymapAsString $ fromJust keymap

   assert (dump == original) "original and dump not equal!"

   let keymap2 = newKeymapFromString ctx dump
   let dump2 = keymapAsString $ fromJust keymap2

   assert (dump2 == dump) "dump and dump2 not equal!"

   return ()
