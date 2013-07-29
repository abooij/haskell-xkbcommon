import Control.Monad
import Data.Maybe
import Data.Functor

import Text.XkbCommon
import Text.XkbCommon.Constants

import Common

blergh :: IO Bool -> IO ()
blergh iob = do
   b <- iob
   assert b "err"

testUpdateKey :: Keymap -> IO ()
testUpdateKey km = do
   st <- newKeymapState km

   updateKeymapState st keycode_leftctrl keyDown
   blergh (stateModNameIsActive st modname_ctrl stateModDepressed)

   updateKeymapState st keycode_rightalt keyDown
   blergh (stateModNameIsActive st modname_ctrl stateModDepressed)
   blergh (stateModNameIsActive st modname_alt stateModDepressed)

   updateKeymapState st keycode_leftctrl keyUp
   blergh (liftM not $ stateModNameIsActive st modname_ctrl stateModDepressed)
   blergh (stateModNameIsActive st modname_alt stateModDepressed)

   updateKeymapState st keycode_rightalt keyUp
   blergh (liftM not $ stateModNameIsActive st modname_alt stateModDepressed)

   updateKeymapState st keycode_capslock keyDown
   blergh (stateModNameIsActive st modname_caps stateModDepressed)
   updateKeymapState st keycode_capslock keyUp
   blergh (liftM not $ stateModNameIsActive st modname_caps stateModDepressed)
   blergh (stateModNameIsActive st modname_caps stateModLocked)
   blergh (stateLedNameIsActive st ledname_caps)
   out <- getStateSyms st keycode_q
   assert (1 == length out) "err"
   assert (out !! 0 == keysym_Q) "err"

   updateKeymapState st keycode_numlock keyDown
   updateKeymapState st keycode_numlock keyUp
   blergh (stateModNameIsActive st modname_caps stateModLocked)
   blergh (stateModNameIsActive st "Mod2" stateModLocked)
   out <- getStateSyms st keycode_kp1
   assert (1 == length out) "err"
   assert (out !! 0 == keysym_KP_1) "err"
   blergh (stateLedNameIsActive st ledname_num)

   updateKeymapState st keycode_numlock keyDown
   updateKeymapState st keycode_numlock keyUp

   updateKeymapState st keycode_compose keyDown
   updateKeymapState st keycode_compose keyUp

   blergh (stateLedNameIsActive st "Group 2")
   out <- stateLedNameIsActive st ledname_num
   assert (not out) "err"

   updateKeymapState st keycode_compose keyDown
   updateKeymapState st keycode_compose keyUp

   updateKeymapState st keycode_capslock keyDown
   updateKeymapState st keycode_capslock keyUp
   blergh (liftM not $ stateModNameIsActive st modname_caps stateModEffective)
   blergh (liftM not $ stateLedNameIsActive st ledname_caps)
   out <- getStateSyms st keycode_q
   assert (1 == length out) "err"
   assert (out !! 0 == keysym_q) "err"


   out <- getStateSyms st keycode_6
   assert (5 == length out) "err"
   assert (out !! 0 == keysym_H) "err"
   assert (out !! 1 == keysym_E) "err"
   assert (out !! 2 == keysym_L) "err"
   assert (out !! 3 == keysym_L) "err"
   assert (out !! 4 == keysym_O) "err"

   out <- getOneKeySym st keycode_6
   assert (out == Nothing) "err"
   updateKeymapState st keycode_6 keyDown
   updateKeymapState st keycode_6 keyUp

   out <- getOneKeySym st keycode_5
   assert (out == Just keysym_5) "err"


{-

static void
test_serialisation(struct xkb_keymap *keymap)
{
    struct xkb_state *state = xkb_state_new(keymap);
    xkb_mod_mask_t base_mods;
    xkb_mod_mask_t latched_mods;
    xkb_mod_mask_t locked_mods;
    xkb_mod_mask_t effective_mods;
    xkb_mod_index_t caps, shift, ctrl;
    xkb_layout_index_t base_group = 0;
    xkb_layout_index_t latched_group = 0;
    xkb_layout_index_t locked_group = 0;

    assert(state);

    caps = xkb_keymap_mod_get_index(keymap, XKB_MOD_NAME_CAPS);
    assert(caps != XKB_MOD_INVALID);
    shift = xkb_keymap_mod_get_index(keymap, XKB_MOD_NAME_SHIFT);
    assert(shift != XKB_MOD_INVALID);
    ctrl = xkb_keymap_mod_get_index(keymap, XKB_MOD_NAME_CTRL);
    assert(ctrl != XKB_MOD_INVALID);

    xkb_state_update_key(state, KEY_CAPSLOCK + EVDEV_OFFSET, XKB_KEY_DOWN);
    xkb_state_update_key(state, KEY_CAPSLOCK + EVDEV_OFFSET, XKB_KEY_UP);
    base_mods = xkb_state_serialize_mods(state, XKB_STATE_MODS_DEPRESSED);
    assert(base_mods == 0);
    latched_mods = xkb_state_serialize_mods(state, XKB_STATE_MODS_LATCHED);
    assert(latched_mods == 0);
    locked_mods = xkb_state_serialize_mods(state, XKB_STATE_MODS_LOCKED);
    assert(locked_mods == (1 << caps));
    effective_mods = xkb_state_serialize_mods(state, XKB_STATE_MODS_EFFECTIVE);
    assert(effective_mods == locked_mods);

    xkb_state_update_key(state, KEY_LEFTSHIFT + EVDEV_OFFSET, XKB_KEY_DOWN);
    base_mods = xkb_state_serialize_mods(state, XKB_STATE_MODS_DEPRESSED);
    assert(base_mods == (1 << shift));
    latched_mods = xkb_state_serialize_mods(state, XKB_STATE_MODS_LATCHED);
    assert(latched_mods == 0);
    locked_mods = xkb_state_serialize_mods(state, XKB_STATE_MODS_LOCKED);
    assert(locked_mods == (1 << caps));
    effective_mods = xkb_state_serialize_mods(state, XKB_STATE_MODS_EFFECTIVE);
    assert(effective_mods == (base_mods | locked_mods));

    base_mods |= (1 << ctrl);
    xkb_state_update_mask(state, base_mods, latched_mods, locked_mods,
                          base_group, latched_group, locked_group);

    assert(xkb_state_mod_index_is_active(state, ctrl, XKB_STATE_MODS_DEPRESSED));
    assert(xkb_state_mod_index_is_active(state, ctrl, XKB_STATE_MODS_EFFECTIVE));

    xkb_state_unref(state);
}

static void
test_repeat(struct xkb_keymap *keymap)
{
    assert(!xkb_keymap_key_repeats(keymap, KEY_LEFTSHIFT + 8));
    assert(xkb_keymap_key_repeats(keymap, KEY_A + 8));
    assert(xkb_keymap_key_repeats(keymap, KEY_8 + 8));
    assert(xkb_keymap_key_repeats(keymap, KEY_DOWN + 8));
    assert(xkb_keymap_key_repeats(keymap, KEY_KBDILLUMDOWN + 8));
}

static void
test_consume(struct xkb_keymap *keymap)
{
    struct xkb_state *state = xkb_state_new(keymap);
    xkb_mod_index_t alt, shift;
    xkb_mod_mask_t mask;

    assert(state);

    alt = xkb_keymap_mod_get_index(keymap, XKB_MOD_NAME_ALT);
    assert(alt != XKB_MOD_INVALID);
    shift = xkb_keymap_mod_get_index(keymap, XKB_MOD_NAME_SHIFT);
    assert(shift != XKB_MOD_INVALID);

    xkb_state_update_key(state, KEY_LEFTALT + EVDEV_OFFSET, XKB_KEY_DOWN);
    xkb_state_update_key(state, KEY_LEFTSHIFT + EVDEV_OFFSET, XKB_KEY_DOWN);
    xkb_state_update_key(state, KEY_EQUAL + EVDEV_OFFSET, XKB_KEY_DOWN);

    fprintf(stderr, "dumping state for Alt-Shift-+\n");
    print_state(state);

    mask = xkb_state_serialize_mods(state, XKB_STATE_MODS_EFFECTIVE);
    assert(mask == ((1 << alt) | (1 << shift)));
    mask = xkb_state_mod_mask_remove_consumed(state, KEY_EQUAL + EVDEV_OFFSET,
                                              mask);
    assert(mask == (1 << alt));

    xkb_state_unref(state);
}

static void
key_iter(struct xkb_keymap *keymap, xkb_keycode_t key, void *data)
{
    int *counter = (int *) data;

    assert(*counter == key);
    (*counter)++;
}

static void
test_range(struct xkb_keymap *keymap)
{
    int counter;

    assert(xkb_keymap_min_keycode(keymap) == 9);
    assert(xkb_keymap_max_keycode(keymap) == 253);

    counter = xkb_keymap_min_keycode(keymap);
    xkb_keymap_key_for_each(keymap, key_iter, &counter);
    assert(counter == xkb_keymap_max_keycode(keymap) + 1);
}
-}

main = do
   ctx <- getTestContext
   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO
         (Just "evdev")
         (Just "pc104")
         (Just "us,ru")
         Nothing
         (Just "grp:menu_toggle"))

   testUpdateKey km
{-   testSerialisation km
   testRepeat km
   testConsume km
   testRange km-}
