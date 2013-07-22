module Common (assert) where

assert :: Bool -> String -> IO ()
assert False str = ioError (userError str)
assert _ _ = return ()

