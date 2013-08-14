import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Control.Monad
import System.Random
import Data.Time.Clock

import Text.XkbCommon

import Common

benchmarkIterations = 20000000

--updateHead :: a -> V.Vector a -> V.Vector a
--updateHead a xs = a:V.tail xs
--update i a xs = V.take i xs ++ updateHead a (V.drop i xs)

update i a xs = xs V.// [(i,a)]

bench :: KeyboardState -> Int -> V.Vector Bool -> IO ()
bench st n keys = unless (n == 0) $ do
   rand <- getStdRandom (randomR (9,255))
   updateKeyboardState st (CKeycode $ fromIntegral rand)
      (if keys V.! rand then keyUp else keyDown)
   return =<< bench st (n-1) $ Main.update rand (not $ keys V.! rand) keys  -- keys // [(rand,(not $ keys V.! rand))]

main = do
   start <- getCurrentTime

   ctx <- getTestContext
   km <- liftM fromJust $ newKeymapFromNames ctx (RMLVO (Just "evdev") (Just "pc104") (Just "us,ru,il,de") (Just ",,,neo") (Just "grp:menu_toggle"))
   st <- newKeyboardState km

   bench st benchmarkIterations (V.replicate 256 False)

   end <- getCurrentTime

   putStrLn $ "bench-key-proc took " ++ show (diffUTCTime end start)

   return ()
