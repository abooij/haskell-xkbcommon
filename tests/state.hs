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
   st <- newKeyboardState km

   updateKeyboardState st keycode_leftctrl keyDown
   blergh (stateModNameIsActive st modname_ctrl stateModDepressed)

   updateKeyboardState st keycode_rightalt keyDown
   blergh (stateModNameIsActive st modname_ctrl stateModDepressed)
   blergh (stateModNameIsActive st modname_alt stateModDepressed)

   updateKeyboardState st keycode_leftctrl keyUp
   blergh (liftM not $ stateModNameIsActive st modname_ctrl stateModDepressed)
   blergh (stateModNameIsActive st modname_alt stateModDepressed)

   updateKeyboardState st keycode_rightalt keyUp
   blergh (liftM not $ stateModNameIsActive st modname_alt stateModDepressed)

   updateKeyboardState st keycode_capslock keyDown
   blergh (stateModNameIsActive st modname_caps stateModDepressed)
   updateKeyboardState st keycode_capslock keyUp
   blergh (liftM not $ stateModNameIsActive st modname_caps stateModDepressed)
   blergh (stateModNameIsActive st modname_caps stateModLocked)
   blergh (stateLedNameIsActive st ledname_caps)
   out <- getStateSyms st keycode_q
   assert (1 == length out) "err"
   assert (head out == keysym_Q) "err"

   updateKeyboardState st keycode_numlock keyDown
   updateKeyboardState st keycode_numlock keyUp
   blergh (stateModNameIsActive st modname_caps stateModLocked)
   blergh (stateModNameIsActive st "Mod2" stateModLocked)
   out <- getStateSyms st keycode_kp1
   assert (1 == length out) "err"
   assert (head out == keysym_KP_1) "err"
   blergh (stateLedNameIsActive st ledname_num)

   updateKeyboardState st keycode_numlock keyDown
   updateKeyboardState st keycode_numlock keyUp

   updateKeyboardState st keycode_compose keyDown
   updateKeyboardState st keycode_compose keyUp

   blergh (stateLedNameIsActive st "Group 2")
   out <- stateLedNameIsActive st ledname_num
   assert (not out) "err"

   updateKeyboardState st keycode_compose keyDown
   updateKeyboardState st keycode_compose keyUp

   updateKeyboardState st keycode_capslock keyDown
   updateKeyboardState st keycode_capslock keyUp
   blergh (liftM not $ stateModNameIsActive st modname_caps stateModEffective)
   blergh (liftM not $ stateLedNameIsActive st ledname_caps)
   out <- getStateSyms st keycode_q
   assert (1 == length out) "err"
   assert (head out == keysym_q) "err"


   out <- getStateSyms st keycode_6
   assert (5 == length out) "err"
   assert (head out == keysym_H) "err"
   assert (out !! 1 == keysym_E) "err"
   assert (out !! 2 == keysym_L) "err"
   assert (out !! 3 == keysym_L) "err"
   assert (out !! 4 == keysym_O) "err"

   out <- getOneKeySym st keycode_6
   assert (isNothing out) "err"
   updateKeyboardState st keycode_6 keyDown
   updateKeyboardState st keycode_6 keyUp

   out <- getOneKeySym st keycode_5
   assert (out == Just keysym_5) "err"


testSerialisation :: Keymap -> IO ()
testSerialisation km = do
   st <- newKeyboardState km

   let caps = fromJust $ keymapModIdx km modname_caps
   let shift = fromJust $ keymapModIdx km modname_shift
   let ctrl = fromJust $ keymapModIdx km modname_ctrl

   updateKeyboardState st keycode_capslock keyDown
   updateKeyboardState st keycode_capslock keyUp

   baseMods <- stateSerializeMods st stateModDepressed
   latchedMods <- stateSerializeMods st stateModLatched
   lockedMods <- stateSerializeMods st stateModLocked
   effectiveMods <- stateSerializeMods st stateModEffective

   assert (baseMods == 0) "baseMods invalid"
   assert (latchedMods == 0) "latchedMods invalid"
   assert (lockedMods == (2^unCModIndex caps)) "lockedMods invalid"
   assert (effectiveMods == lockedMods) "effectiveMods invalid"

   updateKeyboardState st keycode_leftshift keyDown

   baseMods <- stateSerializeMods st stateModDepressed
   latchedMods <- stateSerializeMods st stateModLatched
   lockedMods <- stateSerializeMods st stateModLocked
   effectiveMods <- stateSerializeMods st stateModEffective

   assert (baseMods == (2^unCModIndex shift)) "baseMods invalid"
   assert (latchedMods == 0) "latchedMods invalid"
   assert (lockedMods == (2^unCModIndex caps)) "lockedMods invalid"
   assert (effectiveMods == (2^unCModIndex shift) + (2^unCModIndex caps)) "effectiveMods invalid"

   let baseModsWithCtrl = baseMods + 2^unCModIndex ctrl
   let layout0 = CLayoutIndex 0
   updateKeyboardStateMask st (baseModsWithCtrl, latchedMods, lockedMods) (layout0, layout0, layout0)

   blergh $ stateModIndexIsActive st ctrl stateModDepressed
   blergh $ stateModIndexIsActive st ctrl stateModEffective

testRepeat :: Keymap -> IO ()
testRepeat km = do
   assert (not $ keymapKeyRepeats km keycode_leftshift) "shift key repeat error"
   assert (keymapKeyRepeats km keycode_a) "a key repeat error"
   assert (keymapKeyRepeats km keycode_8) "8 key repeat error"
   assert (keymapKeyRepeats km keycode_down) "down key repeat error"
   assert (keymapKeyRepeats km keycode_kbdillumdown) "kbdillumdown key repeat error"

testConsume :: Keymap -> IO ()
testConsume km = do
   st <- newKeyboardState km

   let alt = fromJust $ keymapModIdx km modname_alt
   let shift = fromJust $ keymapModIdx km modname_shift

   updateKeyboardState st keycode_leftalt keyDown
   updateKeyboardState st keycode_leftshift keyDown
   updateKeyboardState st keycode_equal keyDown

   mask <- stateSerializeMods st stateModEffective
   assert (mask == 2^unCModIndex alt + 2^unCModIndex shift) "err"

   newMask <- stateRemoveConsumed st keycode_equal mask
   assert (newMask == 2^unCModIndex alt) $
               "consumed error: mask " ++ show mask
               ++ ", newMask " ++ show newMask
               ++ ", expected " ++ show (2^unCModIndex alt)

{- UNSUPPORTED AT THIS STAGE
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
   testSerialisation km
   testRepeat km
   testConsume km
{-   testRange km-}
