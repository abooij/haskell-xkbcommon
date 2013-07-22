import Data.Maybe
import Control.Monad

import Text.XkbCommon

import Common

main = do
	ctx <- liftM fromJust $ newContext pureFlags

	appendIncludePath ctx "data/"
	num <- numIncludePaths ctx
	assert (num == 1) "did not find data/ dir"
	appendIncludePath ctx "¡NONSENSE!"
	num <- numIncludePaths ctx
	assert (num == 1) "loaded nonsene dir"
