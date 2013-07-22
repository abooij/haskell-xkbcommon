import Data.Maybe
import Data.Vector.Unboxed as V
import Control.Monad
import System.Random

import Text.XkbCommon

import Common

benchmark_iterations = 20000000

--updateHead :: a -> V.Vector a -> V.Vector a
--updateHead a xs = a:V.tail xs
--update i a xs = V.take i xs ++ updateHead a (V.drop i xs)

update i a xs = xs // [(i,a)]

bench :: KeymapState -> Int -> Vector Bool -> IO ()
bench st n keys = if n == 0 then return () else do
   rand <- getStdRandom (randomR (9,255))
   if keys V.! rand
      then updateKeymapState st (CKeycode $ fromIntegral rand) keyUp
      else updateKeymapState st (CKeycode $ fromIntegral rand) keyDown
   --putStrLn $ show rand
   return =<< bench st (n-1) $ Main.update rand (not $ keys V.! rand) keys  -- keys // [(rand,(not $ keys V.! rand))]

main = do
   ctx <- liftM fromJust $ newContext defaultFlags
   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO (Just "evdev") (Just "pc104") (Just "us,ru,il,de") (Just ",,,neo") (Just "grp:menu_toggle"))
   st <- newKeymapState km

   bench st benchmark_iterations (V.replicate 256 False)

   return ()
