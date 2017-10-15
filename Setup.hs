import Control.Arrow
import Distribution.PackageDescription
import Distribution.Simple hiding (Module)
import Distribution.Simple.LocalBuildInfo
import Language.Preprocessor.Cpphs
import System.FilePath

import Text.XkbCommon.ParseDefines

import Module
import Utils

sourceLoc :: FilePath
sourceLoc = "./"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
       { buildHook = \p l h f -> generateSource sourceLoc >> buildHook simpleUserHooks p l h f
       , haddockHook = \p l h f -> generateSource sourceLoc >> haddockHook simpleUserHooks p l h f
       , sDistHook = \p ml h f -> case ml of
           Nothing -> fail "No local buildinfo available. configure first"
           Just l -> do
             generateSource sourceLoc
             sDistHook simpleUserHooks p ml h f
       }

generateSource :: FilePath -> IO ()
generateSource fp = do
  parsedDefs <- getKeysymDefs
  saveModule fp (keysymsModule parsedDefs)
  return ()

keysymsModule :: [(String,Integer)] -> Module
keysymsModule defs = Module "Text.XkbCommon.KeysymPatterns" [] $
                     Import ["Text.XkbCommon.InternalTypes"] :
                     map (\(name,val) ->
                           Pattern ("Keysym_" ++ name)
                                   Nothing
                                   ("= Keysym " ++ show val))
                         defs
