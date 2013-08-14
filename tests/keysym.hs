import Control.Monad
import Data.Maybe

import Text.XkbCommon
import Text.XkbCommon.KeysymList

import Common

testString :: String -> Maybe Keysym -> IO ()
testString str ck = do
   print $ keysymFromName str
   assert (keysymFromName str == ck) ("Name " ++ str ++ " returned " ++
      show (keysymFromName str) ++" instead of expected keysym " ++ show ck)

testCaselessString :: String -> Maybe Keysym -> IO ()
testCaselessString str ck = assert (keysymFromNameCaseInsensitive str == ck) ("Caseless name " ++ str ++ " returned " ++
      show (keysymFromNameCaseInsensitive str) ++" instead of expected keysym " ++ show ck)

testKeysym :: Keysym -> String -> IO ()
testKeysym ks str = assert (keysymName ks == str) ("Keysym name " ++ keysymName ks ++
      " for keysym " ++ show ks ++ " did not match expected value " ++ str)

testUtf8 :: Keysym -> String -> IO ()
testUtf8 ks str = do
   print $ keysymUtf8 ks
   assert (keysymUtf8 ks == str) ("Keysym " ++ keysymUtf8 ks ++
      " for " ++ show ks ++ " did not match expected value " ++ str)

main = do
   testString "Undo" (Just $ Keysym 0xFF65)
   testString "ThisKeyShouldNotExist" Nothing
   testString "XF86_Switch_VT_5" (Just $ Keysym 0x1008FE05)
   testString "VoidSymbol" (Just $ Keysym 0xFFFFFF)
   testString "U4567" (Just $ Keysym 0x1004567)
   testString "0x10203040" (Just $ Keysym 0x10203040)
   testString "a" (Just $ Keysym 0x61)
   testString "A" (Just $ Keysym 0x41)
   testString "ch" (Just $ Keysym 0xfea0)
   testString "Ch" (Just $ Keysym 0xfea1)
   testString "CH" (Just $ Keysym 0xfea2)
   testString "THORN" (Just $ Keysym 0x00de)
   testString "Thorn" (Just $ Keysym 0x00de)
   testString "thorn" (Just $ Keysym 0x00fe)

   testKeysym (Keysym 0x1008FF56) "XF86Close"
   testKeysym (Keysym 0x1008FE20) "XF86Ungrab"
   testKeysym (Keysym 0x01001234) "U1234"
   testKeysym (Keysym 0x010002DE) "U02DE"
   testKeysym (Keysym 0x0101F4A9) "U0001F4A9"

   testCaselessString "Undo" (Just $ Keysym 0xFF65)
   testCaselessString "UNDO" (Just $ Keysym 0xFF65)
   testCaselessString "A" (Just $ Keysym 0x61)
   testCaselessString "a" (Just $ Keysym 0x61)
   testCaselessString "ThisKeyShouldNotExist" Nothing
   testCaselessString "XF86_Switch_vT_5" (Just $ Keysym 0x1008FE05)
   testCaselessString "xF86_SwitcH_VT_5" (Just $ Keysym 0x1008FE05)
   testCaselessString "xF86SwiTch_VT_5" (Just $ Keysym 0x1008FE05)
   testCaselessString "xF86Switch_vt_5" (Just $ Keysym 0x1008FE05)
   testCaselessString "VoidSymbol" (Just $ Keysym 0xFFFFFF)
   testCaselessString "vOIDsymBol" (Just $ Keysym 0xFFFFFF)
   testCaselessString "U4567" (Just $ Keysym 0x1004567)
   testCaselessString "u4567" (Just $ Keysym 0x1004567)
   testCaselessString "0x10203040" (Just $ Keysym 0x10203040)
   testCaselessString "0X10203040" (Just $ Keysym 0x10203040)
   testCaselessString "THORN" (Just $ Keysym 0x00fe)
   testCaselessString "Thorn" (Just $ Keysym 0x00fe)
   testCaselessString "thorn" (Just $ Keysym 0x00fe)

   testUtf8 keysym_y "y"
   testUtf8 keysym_u "u"
   testUtf8 keysym_m "m"
   testUtf8 keysym_Cyrillic_em "м"
   testUtf8 keysym_Cyrillic_u "у"
   testUtf8 keysym_exclam "!"
   testUtf8 keysym_oslash "ø"
   testUtf8 keysym_hebrew_aleph "א"
   testUtf8 keysym_Arabic_sheen "ش"

   testUtf8 keysym_space " "
   testUtf8 keysym_KP_Space " "
   testUtf8 keysym_BackSpace "\b"
   --testUtf8 keysym_Escape "\033"
   testUtf8 keysym_KP_Separator ","
   testUtf8 keysym_KP_Decimal "."
   testUtf8 keysym_Tab "\t"
   testUtf8 keysym_KP_Tab "\t"
   testUtf8 keysym_hyphen "\173" -- "­"
   testUtf8 keysym_Linefeed "\n"
   testUtf8 keysym_Return "\r"
   testUtf8 keysym_KP_Enter "\r"
   testUtf8 keysym_KP_Equal "="
   testUtf8 keysym_9 "9"
   testUtf8 keysym_KP_9 "9"
   testUtf8 keysym_KP_Multiply "*"
   testUtf8 keysym_KP_Subtract "-"

