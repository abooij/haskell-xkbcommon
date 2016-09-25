module Text.XkbCommon.ParseDefines
   ( readHeader, getKeysymDefs, genKeysyms, genKeycodes, genModnames ) where

import Language.Haskell.TH
import Language.Preprocessor.Cpphs
import System.Process
import Data.List
import Data.Maybe (isJust)
import Data.Text (pack, unpack, toLower)
import Control.Arrow

-- this function calls the c preprocessor to find out what the full path to a header file is.
readHeader :: String -> IO (String, String)
readHeader str = do
  cpp_out <- readProcess "cpp" [] ("#include<" ++ str ++ ">")
  -- parse output:
  let headerfile = read $ head $ map ((!! 2) . words) (filter (isInfixOf str) $ lines cpp_out)
  header <- readFile headerfile
  return (headerfile, header)

getKeysymDefs :: IO [(String,Integer)]
getKeysymDefs = do
  (headerFilename, keysyms_header) <- readHeader "xkbcommon/xkbcommon-keysyms.h"
  (_, defs) <- macroPassReturningSymTab [] defaultBoolOptions [(newfile headerFilename, keysyms_header)]
  let exclude_defs = ["XKB_KEY_VoidSymbol", "XKB_KEY_NoSymbol"]
  let filtered_defs = filter (\ (name, _) -> isPrefixOf "XKB_KEY" name && notElem name exclude_defs) defs
  let parsed_defs = map (drop 8 *** read) filtered_defs
  return parsed_defs

genKeysyms :: IO [Dec]
genKeysyms = do
   parsed_defs <- getKeysymDefs
   return $ map (\ (name, val) -> ValD (VarP $ mkName ("keysym_" ++ name)) (NormalB (AppE (VarE $ mkName "toKeysym") (AppE (ConE $ mkName "CKeysym") $ LitE (IntegerL val)))) []) parsed_defs

genKeycodes :: IO [Dec]
genKeycodes = do
   (headerFilename, keysyms_header) <- readHeader "linux/input.h"
   preprocessed <- cppIfdef headerFilename [] [] defaultBoolOptions keysyms_header
   (_, defs) <- macroPassReturningSymTab [] defaultBoolOptions preprocessed
   let exclude_defs = []
   let filtered_defs = filter (\ (name, val) -> isPrefixOf "KEY_" name && notElem name exclude_defs && isJust (maybeRead val :: Maybe Int)) defs
   let parsed_defs = map (drop 4 *** read) filtered_defs
   return $ map (\ (name, val) -> ValD (VarP $ mkName ("keycode_" ++ lowerCase name)) (NormalB (AppE (ConE $ mkName "CKeycode") $ LitE (IntegerL (8 + val)))) []) parsed_defs

genModnames :: IO [Dec]
-- genKeycodes = return []
genModnames = do
   (headerFilename, keysyms_header) <- readHeader "xkbcommon/xkbcommon-names.h"
   (_, defs) <- macroPassReturningSymTab [] defaultBoolOptions [(newfile headerFilename, keysyms_header)]
   let exclude_defs = []
   let mod_defs = filter (\ (name, val) -> isPrefixOf "XKB_MOD_" name && notElem name exclude_defs && isJust (maybeRead val :: Maybe String)) defs
   let led_defs = filter (\ (name, val) -> isPrefixOf "XKB_LED_" name && notElem name exclude_defs && isJust (maybeRead val :: Maybe String)) defs
   let parsed_mods = map ((\ name -> "modname_" ++ lowerCase (drop 13 name)) *** read) mod_defs
   let parsed_leds = map ((\ name -> "ledname_" ++ lowerCase (drop 13 name)) *** read) led_defs
   return $ map (\ (name, val) -> ValD (VarP $ mkName name) (NormalB (LitE (StringL val))) []) (parsed_mods ++ parsed_leds)

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
   [(x, "")] -> Just x
   _         -> Nothing

lowerCase :: String -> String
lowerCase = unpack . toLower . pack
