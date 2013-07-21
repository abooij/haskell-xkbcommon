import Data.Maybe
import Control.Monad
import Control.Exception

import Text.XkbCommon

main = do
	ctx <- liftM fromJust $ newContext pureFlags

	appendIncludePath ctx "data/"
	num <- numIncludePaths ctx
	appendIncludePath (assert (num == 1) ctx) "¡NONSENSE!"
	num <- numIncludePaths ctx
	return $ assert (num == 1) ()
