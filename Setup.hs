import Control.Arrow
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Language.Preprocessor.Cpphs
import System.FilePath

import Text.XkbCommon.ParseDefines

import Module
import Utils

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
       { buildHook = \p l h f -> generateAPI l >> buildHook simpleUserHooks p l h f
       , haddockHook = \p l h f -> generateAPI l >> haddockHook simpleUserHooks p l h f
       , sDistHook = \p ml h f -> case ml of
           Nothing -> fail "No local buildinfo available. configure first"
           Just l -> do
             let editlib lib = lib { libBuildInfo = editBuildInfo (libBuildInfo lib) }
                 editBuildInfo bi = bi { hsSourceDirs = (buildDir l </> "autogen") : hsSourceDirs bi }
                 p' = p { library = fmap editlib (library p) }
             generateAPI l >> sDistHook simpleUserHooks p' ml h f
       }

generateAPI :: LocalBuildInfo -> IO ()
generateAPI l = do
  generateSource (buildDir l </> "autogen")
  return ()

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
