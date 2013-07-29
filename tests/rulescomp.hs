import Control.Monad
import Data.Maybe
import Data.Functor
import System.Posix.Env

import Text.XkbCommon
import Text.XkbCommon.Constants

import Common


setRmlvoEnv :: RMLVO -> IO ()
setRmlvoEnv rmlvo = do
   procEnv "XKB_DEFAULT_RULES" rules
   procEnv "XKB_DEFAULT_MODEL" model
   procEnv "XKB_DEFAULT_LAYOUT" layout
   procEnv "XKB_DEFAULT_VARIANT" variant
   procEnv "XKB_DEFAULT_OPTIONS" options
      where
         procEnv :: String -> (RMLVO -> Maybe String) -> IO ()
         procEnv envName getter = case getter rmlvo of
                                     Just x -> setEnv envName x True
                                     Nothing -> unsetEnv envName

main = do
   ctx <- getTestContext
   envCtx <- liftM fromJust $ newContext contextNoDefaultIncs
   appendIncludePath envCtx datadir
   setRmlvoEnv noPrefs

   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO
         (Just "evdev")
         (Just "pc105")
         (Just "us,il,ru,ca")
         (Just ",,,multix")
         (Just "grp:alts_toggle,ctrl:nocaps,compose:rwin"))
   testKeySeq km [
      (keycode_q,          Both, keysym_q),
      (keycode_leftalt,    Down, keysym_Alt_L),
      (keycode_rightalt,   Down, keysym_ISO_Next_Group),
      (keycode_rightalt,   Up,   keysym_ISO_Level3_Shift),
      (keycode_leftalt,    Up,   keysym_Alt_L),
      (keycode_q,          Both, keysym_slash),
      (keycode_leftshift,  Down, keysym_Shift_L),
      (keycode_q,          Both, keysym_Q),
      (keycode_rightmeta,  Both, keysym_Multi_key)]

   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO
         (Just "evdev")
         (Just "pc105")
         (Just "us,in")
         Nothing
         (Just "grp:alts_toggle"))
   testKeySeq km [
      (keycode_a,          Both, keysym_a),
      (keycode_leftalt,    Down, keysym_Alt_L),
      (keycode_rightalt,   Down, keysym_ISO_Next_Group),
      (keycode_rightalt,   Up,   keysym_ISO_Level3_Shift),
      (keycode_leftalt,    Up,   keysym_Alt_L),
      (keycode_a,          Both, ks "U094b")]

   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO
         (Just "evdev")
         (Just "pc105")
         (Just "us")
         (Just "intl")
         Nothing)
   testKeySeq km [
      (keycode_grave,      Both,  keysym_dead_grave)]

   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO
         (Just "evdev")
         (Just "pc105")
         (Just "us")
         (Just "intl")
         (Just "grp:alts_toggle"))
   testKeySeq km [
      (keycode_grave,      Both,  keysym_dead_grave)]

   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO
         (Just "evdev")
         Nothing
         (Just "us:20")
         Nothing
         Nothing)
   testKeySeq km [
      (keycode_a,          Both, keysym_a)]

   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO
         (Just "evdev")
         Nothing
         (Just "us,,ca")
         Nothing
         (Just "grp:alts_toggle"))
   testKeySeq km [
      (keycode_a,          Both, keysym_a),
      (keycode_leftalt,    Down, keysym_Alt_L),
      (keycode_rightalt,   Down, keysym_ISO_Next_Group),
      (keycode_rightalt,   Up,   keysym_ISO_Next_Group),
      (keycode_leftalt,    Up,   keysym_Alt_L),
      (keycode_leftalt,    Down, keysym_Alt_L),
      (keycode_rightalt,   Down, keysym_ISO_Next_Group),
      (keycode_rightalt,   Up,   keysym_ISO_Level3_Shift),
      (keycode_leftalt,    Up,   keysym_Alt_L),
      (keycode_apostrophe, Both, keysym_dead_grave)]

   km <- liftM fromJust $ newKeymapFromNames ctx noPrefs
   testKeySeq km [
      (keycode_a,          Both, keysym_a)]

   km <- newKeymapFromNames ctx (RMLVO
         (Just "does-not-exist")
         Nothing
         Nothing
         Nothing
         Nothing)
   assert (isNothing km) "compiled nonexistent keymap"

   setRmlvoEnv (RMLVO
         (Just "evdev")
         Nothing
         (Just "us")
         Nothing
         Nothing)
   km <- liftM fromJust $ newKeymapFromNames envCtx noPrefs
   testKeySeq km [
      (keycode_a,          Both, keysym_a)]

   setRmlvoEnv (RMLVO
         (Just "evdev")
         Nothing
         (Just "us")
         Nothing
         (Just "ctrl:nocaps"))
   km <- liftM fromJust $ newKeymapFromNames envCtx noPrefs
   testKeySeq km [
      (keycode_capslock,   Both, keysym_Control_L)]

   setRmlvoEnv (RMLVO
         (Just "evdev")
         Nothing
         (Just "us,ca")
         (Just ",,,multix")
         (Just "grp:alts_toggle"))
   km <- liftM fromJust $ newKeymapFromNames envCtx noPrefs
   testKeySeq km [
      (keycode_a,          Both, keysym_a),
      (keycode_leftalt,    Down, keysym_Alt_L),
      (keycode_rightalt,   Down, keysym_ISO_Next_Group),
      (keycode_rightalt,   Up,   keysym_ISO_Level3_Shift),
      (keycode_leftalt,    Up,   keysym_Alt_L),
      (keycode_grave,      Up,   keysym_numbersign)]

   setRmlvoEnv (RMLVO
         (Just "broken")
         (Just "what-on-earth")
         (Just "invalid")
         Nothing
         Nothing)
   km <- newKeymapFromNames envCtx noPrefs
   assert (isNothing km) "compiled nonexistent keymap"

