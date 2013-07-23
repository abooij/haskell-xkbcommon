module Text.XkbCommon.ParseDefines
   ( readHeader, genKeysyms, genKeycodes ) where

import Language.Haskell.TH
import Language.Preprocessor.Cpphs
import System.Process
import Data.List
import Data.Maybe (isJust)
import Data.Text (pack, unpack, toLower)

-- this function calls the c preprocessor to find out what the full path to a header file is.
readHeader :: String -> IO (String,String)
readHeader str = do
   cpp_out <- readProcess "cpp" [] ("#include<"++str++">")
   --parse output:
   let headerfile = read $ head $ map ((!! 2) . words) (filter (isInfixOf str) $ lines cpp_out)
   header <- readFile headerfile
   return (headerfile,header)

genKeysyms :: IO [Dec]
genKeysyms = do
   (filename,keysyms_header) <- readHeader "xkbcommon/xkbcommon-keysyms.h"
   (_,defs) <- macroPassReturningSymTab [] defaultBoolOptions [(newfile filename,keysyms_header)]
   let exclude_defs = ["XKB_KEY_VoidSymbol", "XKB_KEY_NoSymbol"]
   let filtered_defs = filter (\ (name,val) -> isPrefixOf "XKB_KEY" name && notElem name exclude_defs) defs
   let parsed_defs = map (\ (name, val) -> (drop 7 name, read val)) filtered_defs
   return $ map (\ (name, val) -> ValD (VarP $ mkName ("xkb_key"++name)) (NormalB (AppE (ConE $ mkName "CKeysym") $ LitE (IntegerL val))) []) parsed_defs

genKeycodes :: IO [Dec]
-- genKeycodes = return []
genKeycodes = do
   (filename,keysyms_header) <- readHeader "linux/input.h"
   (_,defs) <- macroPassReturningSymTab [] defaultBoolOptions [(newfile filename,keysyms_header)]
   let exclude_defs = []
   let filtered_defs = filter (\ (name,val) -> isPrefixOf "KEY_" name && notElem name exclude_defs && (isJust $ (maybeRead val :: Maybe Int))) defs
   let parsed_defs = map (\ (name, val) -> (drop 3 name, read val)) filtered_defs
   return $ map (\ (name, val) -> ValD (VarP $ mkName ("evdev_key"++name)) (NormalB (AppE (ConE $ mkName "CKeycode") $ LitE (IntegerL val))) []) parsed_defs

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
   [(x, "")] -> Just x
   _         -> Nothing