{-# LANGUAGE LambdaCase #-}
module Jatko where
import Jatko.Compiler
import Jatko.Core
import Jatko.Syntax
import Jatko.TypeCheck
import Text.Trifecta
import Control.Monad.Trans.Free
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Reader

typecheck :: FilePath -> IO ()
typecheck path = parseFromFile parseWhole "examples/test.jtk" >>= \case
  Just decs -> do
    print $ decConstructors decs
    forM_ (M.keys $ decVars decs) $ \k -> do
      putStr $ k ++ ": "
      r <- runTC decs $ do
        env <- ask
        resolve decs $ wrap $ Unbound k env pure
      print r
  Nothing -> return ()
