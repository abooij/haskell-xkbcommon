module Text.XkbCommon.ParseDefines
   ( readHeader, genKeysyms ) where

import Language.Haskell.TH
import Language.Preprocessor.Cpphs
import System.Process
import Data.List
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
   return $ map (\ (name, val) -> ValD (VarP $ mkName ("xkb_key"++drop 7 name)) (NormalB (AppE (ConE $ mkName "CKeysym") $ LitE (IntegerL (read val)))) []) $ filter ((/="").snd) defs

