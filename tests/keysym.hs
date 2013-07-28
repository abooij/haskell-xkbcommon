import Text.XkbCommon
import Control.Monad
import Data.Maybe

import Common

testString :: String -> Maybe Keysym -> IO ()
testString str ck = do
   print $ keysymFromName str
   assert (keysymFromName str == ck) ("Name " ++ str ++ " returned " ++
      show (keysymFromName str) ++" instead of expected keysym " ++ show ck)

testCaselessString :: String -> Maybe Keysym -> IO ()
testCaselessString str ck = assert (keysymFromCaselessName str == ck) ("Caseless name " ++ str ++ " returned " ++
      show (keysymFromCaselessName str) ++" instead of expected keysym " ++ show ck)

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

{-
    assert(test_casestring("Undo", 0xFF65
    assert(test_casestring("UNDO", 0xFF65
    assert(test_casestring("A", 0x61
    assert(test_casestring("a", 0x61
    assert(test_casestring("ThisKeyShouldNotExist", XKB_KEY_NoSymbol
    assert(test_casestring("XF86_Switch_vT_5", 0x1008FE05
    assert(test_casestring("xF86_SwitcH_VT_5", 0x1008FE05
    assert(test_casestring("xF86SwiTch_VT_5", 0x1008FE05
    assert(test_casestring("xF86Switch_vt_5", 0x1008FE05
    assert(test_casestring("VoidSymbol", 0xFFFFFF
    assert(test_casestring("vOIDsymBol", 0xFFFFFF
    assert(test_casestring("U4567", 0x1004567
    assert(test_casestring("u4567", 0x1004567
    assert(test_casestring("0x10203040", 0x10203040
    assert(test_casestring("0X10203040", 0x10203040
    assert(test_casestring("THORN", 0x00fe
    assert(test_casestring("Thorn", 0x00fe
    assert(test_casestring("thorn", 0x00fe

    assert(test_utf8(XKB_KEY_y, "y"
    assert(test_utf8(XKB_KEY_u, "u"
    assert(test_utf8(XKB_KEY_m, "m"
    assert(test_utf8(XKB_KEY_Cyrillic_em, "м"
    assert(test_utf8(XKB_KEY_Cyrillic_u, "у"
    assert(test_utf8(XKB_KEY_exclam, "!"
    assert(test_utf8(XKB_KEY_oslash, "ø"
    assert(test_utf8(XKB_KEY_hebrew_aleph, "א"
    assert(test_utf8(XKB_KEY_Arabic_sheen, "ش"

    assert(test_utf8(XKB_KEY_space, " "
    assert(test_utf8(XKB_KEY_KP_Space, " "
    assert(test_utf8(XKB_KEY_BackSpace, "\b"
    assert(test_utf8(XKB_KEY_Escape, "\033"
    assert(test_utf8(XKB_KEY_KP_Separator, ","
    assert(test_utf8(XKB_KEY_KP_Decimal, "."
    assert(test_utf8(XKB_KEY_Tab, "\t"
    assert(test_utf8(XKB_KEY_KP_Tab, "\t"
    assert(test_utf8(XKB_KEY_hyphen, "­"
    assert(test_utf8(XKB_KEY_Linefeed, "\n"
    assert(test_utf8(XKB_KEY_Return, "\r"
    assert(test_utf8(XKB_KEY_KP_Enter, "\r"
    assert(test_utf8(XKB_KEY_KP_Equal, "="
    assert(test_utf8(XKB_KEY_9, "9"
    assert(test_utf8(XKB_KEY_KP_9, "9"
    assert(test_utf8(XKB_KEY_KP_Multiply, "*"
    assert(test_utf8(XKB_KEY_KP_Subtract, "-"

    assert(xkb_keysym_is_lower(XKB_KEY_a
    assert(xkb_keysym_is_lower(XKB_KEY_Greek_lambda
    assert(xkb_keysym_is_lower(xkb_keysym_from_name("U03b1", 0)
    assert(xkb_keysym_is_lower(xkb_keysym_from_name("U03af", 0)

    assert(xkb_keysym_is_upper(XKB_KEY_A
    assert(xkb_keysym_is_upper(XKB_KEY_Greek_LAMBDA
    assert(xkb_keysym_is_upper(xkb_keysym_from_name("U0391", 0)
    assert(xkb_keysym_is_upper(xkb_keysym_from_name("U0388", 0)

    assert(!xkb_keysym_is_upper(XKB_KEY_a
    assert(!xkb_keysym_is_lower(XKB_KEY_A
    assert(!xkb_keysym_is_lower(XKB_KEY_Return
    assert(!xkb_keysym_is_upper(XKB_KEY_Return
    assert(!xkb_keysym_is_lower(XKB_KEY_hebrew_aleph
    assert(!xkb_keysym_is_upper(XKB_KEY_hebrew_aleph
    assert(!xkb_keysym_is_upper(xkb_keysym_from_name("U05D0", 0)
    assert(!xkb_keysym_is_lower(xkb_keysym_from_name("U05D0", 0)
    assert(!xkb_keysym_is_lower(XKB_KEY_8
    assert(!xkb_keysym_is_upper(XKB_KEY_8

    assert(xkb_keysym_is_keypad(XKB_KEY_KP_Enter
    assert(xkb_keysym_is_keypad(XKB_KEY_KP_6
    assert(xkb_keysym_is_keypad(XKB_KEY_KP_Add
    assert(!xkb_keysym_is_keypad(XKB_KEY_Num_Lock
    assert(!xkb_keysym_is_keypad(XKB_KEY_1
    assert(!xkb_keysym_is_keypad(XKB_KEY_Return

    return 0;
}
-}
