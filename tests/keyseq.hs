import Text.XkbCommon
import Control.Monad
import Data.Maybe

import Common

data Direction = Down | Up | Both | Repeat deriving (Show, Eq)

testKeySeq :: Keymap -> [(CKeycode, Direction, CKeysym)] -> IO [()]
testKeySeq km tests = do
   st <- newKeymapState km
   return =<< mapM (testOne st) (zip tests [1..]) where
      testOne st ((kc, dir, ks),n) = do
         -- todo xkb_state_key_get_syms
         syms <- getStateSyms st kc

         when (dir == Down || dir == Both) $ updateKeymapState st kc keyDown >> return ()
         when (dir == Up || dir == Both) $ updateKeymapState st kc keyUp >> return ()

         -- in this test, we always get exactly one keysym
         assert (length syms == 1) "did not get right amount of keysyms"

         assert (head syms == ks) ("did not get correct keysym "++show ks++" for keycode "++show kc++", got "++(show$head syms)++" in test "++show n)

         -- TODO assert keysym names are equal
         return ()


ks = keysymFromName

main = do
   ctx <- getTestContext
   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO
               (Just "evdev")
               (Just "evdev")
               (Just "us,il,ru,de")
               (Just ",,phonetic,neo")
               (Just "grp:alt_shift_toggle,grp:menu_toggle"))

   testKeySeq km [
      (keycode_h,  Both,  keysym_h),
      (keycode_e,  Both,  keysym_e),
      (keycode_l,  Both,  keysym_l),
      (keycode_l,  Both,  keysym_l),
      (keycode_o,  Both,  keysym_o)]
   testKeySeq km [
      (keycode_h,          Both,  keysym_h),
      (keycode_leftshift,  Down,  keysym_Shift_L),
      (keycode_e,          Both,  keysym_E),
      (keycode_l,          Both,  keysym_L),
      (keycode_leftshift,  Up,    keysym_Shift_L),
      (keycode_l,          Both,  keysym_l),
      (keycode_o,          Both,  keysym_o)]
   testKeySeq km [
      (keycode_h,           Down,    keysym_h),
      (keycode_h,           Repeat,  keysym_h),
      (keycode_h,           Repeat,  keysym_h),
      (keycode_leftshift,   Down,    keysym_Shift_L),
      (keycode_h,           Repeat,  keysym_H),
      (keycode_h,           Repeat,  keysym_H),
      (keycode_leftshift,   Up,      keysym_Shift_L),
      (keycode_h,           Repeat,  keysym_h),
      (keycode_h,           Repeat,  keysym_h),
      (keycode_h,           Up,      keysym_h),
      (keycode_h,           Both,    keysym_h)]
   testKeySeq km [
      (keycode_h,          Both,  keysym_h),
      (keycode_leftshift,  Down,  keysym_Shift_L),
      (keycode_e,          Both,  keysym_E),
      (keycode_l,          Both,  keysym_L),
      (keycode_leftshift,  Down,  keysym_Shift_L),
      (keycode_l,          Both,  keysym_L),
      (keycode_o,          Both,  keysym_O)]
   testKeySeq km [
      (keycode_h,           Both,  keysym_h),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_e,           Both,  keysym_E),
      (keycode_l,           Both,  keysym_L),
      (keycode_rightshift,  Up,    keysym_Shift_R),
      (keycode_l,           Both,  keysym_L),
      (keycode_o,           Both,  keysym_O)]
   testKeySeq km [
      (keycode_h,           Both,  keysym_h),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_e,           Both,  keysym_E),
      (keycode_rightshift,  Down,  keysym_Shift_R),
      (keycode_l,           Both,  keysym_L),
      (keycode_rightshift,  Up,    keysym_Shift_R),
      (keycode_l,           Both,  keysym_L),
      (keycode_leftshift,   Up,    keysym_Shift_L),
      (keycode_o,           Both,  keysym_o)]
   testKeySeq km [
      (keycode_h,           Both,  keysym_h),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_h,           Both,  keysym_H),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_h,           Both,  keysym_H),
      (keycode_leftshift,   Up,    keysym_Shift_L),
      (keycode_h,           Both,  keysym_H),
      (keycode_leftshift,   Up,    keysym_Shift_L),
      (keycode_h,           Both,  keysym_h)]
   testKeySeq km [
      (keycode_h,           Both,  keysym_h),
      (keycode_capslock,    Down,  keysym_Caps_Lock),
      (keycode_h,           Both,  keysym_H),
      (keycode_capslock,    Down,  keysym_Caps_Lock),
      (keycode_h,           Both,  keysym_H),
      (keycode_capslock,    Up,    keysym_Caps_Lock),
      (keycode_h,           Both,  keysym_H),
      (keycode_capslock,    Up,    keysym_Caps_Lock),
      (keycode_h,           Both,  keysym_H),
      (keycode_capslock,    Both,  keysym_Caps_Lock),
      (keycode_h,           Both,  keysym_h)]
   testKeySeq km [
      (keycode_h,        Both,  keysym_h),
      (keycode_e,        Both,  keysym_e),
      (keycode_compose,  Both,  keysym_ISO_Next_Group),
      (keycode_k,        Both,  keysym_hebrew_lamed),
      (keycode_f,        Both,  keysym_hebrew_kaph),
      (keycode_compose,  Both,  keysym_ISO_Next_Group),
      (keycode_compose,  Both,  keysym_ISO_Next_Group),
      (keycode_compose,  Both,  keysym_ISO_Next_Group),
      (keycode_o,        Both,  keysym_o)]
   testKeySeq km [
      (keycode_leftshift, Down, keysym_Shift_L),
      (keycode_leftalt,   Down, keysym_ISO_Next_Group),
      (keycode_leftalt,   Up,   keysym_ISO_Next_Group),
      (keycode_leftshift, Up,   keysym_Shift_L)]
   testKeySeq km [
      (keycode_leftalt,   Down, keysym_Alt_L),
      (keycode_leftshift, Down, keysym_ISO_Next_Group),
      (keycode_leftshift, Up,   keysym_ISO_Next_Group),
      (keycode_leftalt,   Up,   keysym_Alt_L)]
   testKeySeq km [
      (keycode_capslock,  Both,  keysym_Caps_Lock),
      (keycode_h,         Both,  keysym_H),
      (keycode_e,         Both,  keysym_E),
      (keycode_l,         Both,  keysym_L),
      (keycode_l,         Both,  keysym_L),
      (keycode_o,         Both,  keysym_O)]
   testKeySeq km [
      (keycode_h,         Both,  keysym_h),
      (keycode_e,         Both,  keysym_e),
      (keycode_capslock,  Both,  keysym_Caps_Lock),
      (keycode_l,         Both,  keysym_L),
      (keycode_l,         Both,  keysym_L),
      (keycode_capslock,  Both,  keysym_Caps_Lock),
      (keycode_o,         Both,  keysym_o)]
   testKeySeq km [
      (keycode_h,         Both,  keysym_h),
      (keycode_capslock,  Down,  keysym_Caps_Lock),
      (keycode_e,         Both,  keysym_E),
      (keycode_l,         Both,  keysym_L),
      (keycode_l,         Both,  keysym_L),
      (keycode_capslock,  Up,    keysym_Caps_Lock),
      (keycode_o,         Both,  keysym_O)]
   testKeySeq km [
      (keycode_h,         Both,  keysym_h),
      (keycode_e,         Both,  keysym_e),
      (keycode_capslock,  Up,    keysym_Caps_Lock),
      (keycode_l,         Both,  keysym_l),
      (keycode_l,         Both,  keysym_l),
      (keycode_o,         Both,  keysym_o)]
   testKeySeq km [
      (keycode_kp1,      Both,  keysym_KP_End),
      (keycode_numlock,  Both,  keysym_Num_Lock),
      (keycode_kp1,      Both,  keysym_KP_1),
      (keycode_kp2,      Both,  keysym_KP_2),
      (keycode_numlock,  Both,  keysym_Num_Lock),
      (keycode_kp2,      Both,  keysym_KP_Down)]
   testKeySeq km [
      (keycode_compose,     Both,  keysym_ISO_Next_Group),
      (keycode_compose,     Both,  keysym_ISO_Next_Group),
      (keycode_1,           Both,  keysym_1),
      (keycode_q,           Both,  keysym_Cyrillic_ya),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_1,           Both,  keysym_exclam),
      (keycode_q,           Both,  keysym_Cyrillic_YA),
      (keycode_leftshift,   Up,    keysym_Shift_L),
      (keycode_v,           Both,  keysym_Cyrillic_zhe),
      (keycode_capslock,    Both,  keysym_Caps_Lock),
      (keycode_1,           Both,  keysym_1),
      (keycode_v,           Both,  keysym_Cyrillic_ZHE),
      (keycode_rightshift,  Down,  keysym_Shift_R),
      (keycode_v,           Both,  keysym_Cyrillic_zhe),
      (keycode_rightshift,  Up,    keysym_Shift_R),
      (keycode_v,           Both,  keysym_Cyrillic_ZHE)]
   testKeySeq km [
      (keycode_compose,     Both,  keysym_ISO_Next_Group),
      (keycode_compose,     Both,  keysym_ISO_Next_Group),
      (keycode_compose,     Both,  keysym_ISO_Next_Group),
      (keycode_1,           Both,  keysym_1),
      (keycode_q,           Both,  keysym_x),
      (keycode_kp7,         Both,  keysym_KP_7),
      (keycode_esc,         Both,  keysym_Escape),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_1,           Both,  keysym_degree),
      (keycode_q,           Both,  keysym_X),
      (keycode_kp7,         Both,  (ks "U2714")),
      (keycode_esc,         Both,  keysym_Escape),
      (keycode_leftshift,   Up,    keysym_Caps_Lock),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_rightshift,  Both,  keysym_Caps_Lock),
      (keycode_leftshift,   Up,    keysym_Caps_Lock),
      (keycode_6,           Both,  keysym_6),
      (keycode_h,           Both,  keysym_S),
      (keycode_kp3,         Both,  keysym_KP_3),
      (keycode_esc,         Both,  keysym_Escape),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_rightshift,  Both,  keysym_Caps_Lock),
      (keycode_leftshift,   Up,    keysym_Caps_Lock),
      (keycode_capslock,    Down,  keysym_ISO_Level3_Shift),
      (keycode_6,           Both,  keysym_cent),
      (keycode_q,           Both,  keysym_ellipsis),
      (keycode_kp7,         Both,  (ks "U2195")),
      (keycode_esc,         Both,  keysym_Escape),
      (keycode_capslock,    Up,    keysym_ISO_Level3_Shift),
      (keycode_capslock,    Down,  keysym_ISO_Level3_Shift),
      (keycode_leftshift,   Down,  keysym_Shift_L),
      (keycode_5,           Both,  keysym_malesymbol),
      (keycode_e,           Both,  keysym_Greek_lambda),
      (keycode_space,       Both,  keysym_nobreakspace),
      (keycode_kp8,         Both,  keysym_intersection),
      (keycode_esc,         Both,  keysym_Escape),
      (keycode_leftshift,   Up,    keysym_Caps_Lock),
      (keycode_capslock,    Up,    keysym_ISO_Level3_Shift),
      (keycode_rightalt,    Down,  keysym_ISO_Level5_Shift),
      (keycode_5,           Both,  keysym_periodcentered),
      (keycode_e,           Both,  keysym_Up),
      (keycode_space,       Both,  keysym_KP_0),
      (keycode_kp8,         Both,  keysym_KP_Up),
      (keycode_esc,         Both,  keysym_Escape),
      (keycode_rightalt,    Up,    keysym_ISO_Level5_Shift),
      (keycode_v,           Both,    keysym_p)]
   return ()

-- TODO translate the rest below...

{-
    xkb_keymap_unref(keymap);
    assert(ctx);
    keymap = test_compile_rules(ctx, "evdev", "", "us,il,ru", "",
                                "grp:alt_shift_toggle_bidir,grp:menu_toggle");
    assert(keymap);

    assert(test_key_seq(keymap,
                        KEY_LEFTSHIFT, DOWN, XKB_KEY_Shift_L,        NEXT,
                        KEY_LEFTALT,   DOWN, XKB_KEY_ISO_Prev_Group, NEXT,
                        KEY_LEFTALT,   UP,   XKB_KEY_ISO_Prev_Group, NEXT,
                        KEY_LEFTSHIFT, UP,   XKB_KEY_Shift_L,        FINISH));

    assert(test_key_seq(keymap,
                        KEY_LEFTALT,   DOWN, XKB_KEY_Alt_L,          NEXT,
                        KEY_LEFTSHIFT, DOWN, XKB_KEY_ISO_Prev_Group, NEXT,
                        KEY_LEFTSHIFT, UP,   XKB_KEY_ISO_Prev_Group, NEXT,
                        KEY_LEFTALT,   UP,   XKB_KEY_Alt_L,          FINISH));

    /* Check backwards (negative) group switching and wrapping. */
    assert(test_key_seq(keymap,
                        KEY_H,         BOTH, XKB_KEY_h,              NEXT,
                        KEY_COMPOSE,   BOTH, XKB_KEY_ISO_Next_Group, NEXT,
                        KEY_H,         BOTH, XKB_KEY_hebrew_yod,     NEXT,
                        KEY_COMPOSE,   BOTH, XKB_KEY_ISO_Next_Group, NEXT,
                        KEY_H,         BOTH, XKB_KEY_Cyrillic_er,    NEXT,
                        KEY_LEFTSHIFT, DOWN, XKB_KEY_Shift_L,        NEXT,
                        KEY_LEFTALT,   BOTH, XKB_KEY_ISO_Prev_Group, NEXT,
                        KEY_LEFTSHIFT, UP,   XKB_KEY_Shift_L,        NEXT,
                        KEY_H,         BOTH, XKB_KEY_hebrew_yod,     NEXT,
                        KEY_LEFTSHIFT, DOWN, XKB_KEY_Shift_L,        NEXT,
                        KEY_LEFTALT,   BOTH, XKB_KEY_ISO_Prev_Group, NEXT,
                        KEY_LEFTSHIFT, UP,   XKB_KEY_Shift_L,        NEXT,
                        KEY_H,         BOTH, XKB_KEY_h,              NEXT,
                        KEY_LEFTSHIFT, DOWN, XKB_KEY_Shift_L,        NEXT,
                        KEY_LEFTALT,   BOTH, XKB_KEY_ISO_Prev_Group, NEXT,
                        KEY_LEFTSHIFT, UP,   XKB_KEY_Shift_L,        NEXT,
                        KEY_H,         BOTH, XKB_KEY_Cyrillic_er,    NEXT,
                        KEY_COMPOSE,   BOTH, XKB_KEY_ISO_Next_Group, NEXT,
                        KEY_H,         BOTH, XKB_KEY_h,              FINISH));

    xkb_keymap_unref(keymap);
    keymap = test_compile_rules(ctx, "evdev", "", "us,il,ru", "",
                                "grp:switch,grp:lswitch,grp:menu_toggle");
    assert(keymap);

    /* Test depressed group works (Mode_switch). */
    assert(test_key_seq(keymap,
                        KEY_H,         BOTH, XKB_KEY_h,                 NEXT,
                        KEY_RIGHTALT,  DOWN, XKB_KEY_Mode_switch,       NEXT,
                        KEY_H,         BOTH, XKB_KEY_hebrew_yod,        NEXT,
                        KEY_RIGHTALT,  UP,   XKB_KEY_ISO_Level3_Shift,  NEXT,
                        KEY_H,         BOTH, XKB_KEY_h,                 NEXT,
                        KEY_RIGHTALT,  DOWN, XKB_KEY_Mode_switch,       NEXT,
                        KEY_H,         BOTH, XKB_KEY_hebrew_yod,        NEXT,
                        KEY_RIGHTALT,  UP,   XKB_KEY_ISO_Level3_Shift,  NEXT,
                        KEY_H,         BOTH, XKB_KEY_h,                 FINISH));

    /* Test locked+depressed group works, with wrapping and accumulation. */
    assert(test_key_seq(keymap,
                        KEY_H,         BOTH, XKB_KEY_h,                 NEXT,
                        KEY_COMPOSE,   BOTH, XKB_KEY_ISO_Next_Group,    NEXT,
                        KEY_LEFTALT,   DOWN, XKB_KEY_Mode_switch,       NEXT,
                        KEY_H,         BOTH, XKB_KEY_Cyrillic_er,       NEXT,
                        KEY_LEFTALT,   UP,   XKB_KEY_Mode_switch,       NEXT,
                        KEY_H,         BOTH, XKB_KEY_hebrew_yod,        NEXT,
                        KEY_COMPOSE,   BOTH, XKB_KEY_ISO_Next_Group,    NEXT,
                        KEY_LEFTALT,   DOWN, XKB_KEY_Mode_switch,       NEXT,
                        /* Should wrap back to first group. */
                        KEY_H,         BOTH, XKB_KEY_h,                 NEXT,
                        KEY_LEFTALT,   UP,   XKB_KEY_Mode_switch,       NEXT,
                        KEY_H,         BOTH, XKB_KEY_Cyrillic_er,       NEXT,
                        KEY_COMPOSE,   BOTH, XKB_KEY_ISO_Next_Group,    NEXT,
                        KEY_H,         BOTH, XKB_KEY_h,                 NEXT,
                        /* Two SetGroup(+1)'s should add up. */
                        KEY_RIGHTALT,  DOWN, XKB_KEY_Mode_switch,       NEXT,
                        KEY_H,         BOTH, XKB_KEY_hebrew_yod,        NEXT,
                        KEY_LEFTALT,   DOWN, XKB_KEY_Mode_switch,       NEXT,
                        KEY_H,         BOTH, XKB_KEY_Cyrillic_er,       NEXT,
                        KEY_LEFTALT,   UP,   XKB_KEY_Mode_switch,       NEXT,
                        KEY_H,         BOTH, XKB_KEY_hebrew_yod,        NEXT,
                        KEY_RIGHTALT,  UP,   XKB_KEY_ISO_Level3_Shift,  NEXT,
                        KEY_H,         BOTH, XKB_KEY_h,                 FINISH));

    xkb_keymap_unref(keymap);
    keymap = test_compile_file(ctx, "keymaps/unbound-vmod.xkb");
    assert(keymap);

    assert(test_key_seq(keymap,
                        KEY_H,         BOTH, XKB_KEY_h,                 NEXT,
                        KEY_Z,         BOTH, XKB_KEY_y,                 NEXT,
                        KEY_MINUS,     BOTH, XKB_KEY_ssharp,            NEXT,
                        KEY_Z,         BOTH, XKB_KEY_y,                 FINISH));

    xkb_keymap_unref(keymap);
    xkb_context_unref(ctx);
    return 0;
}

-}
