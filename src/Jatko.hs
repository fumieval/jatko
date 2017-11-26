{-# LANGUAGE LambdaCase #-}
module Jatko where
import Jatko.Compiler
import Jatko.Core
import Jatko.Syntax
import Jatko.TypeCheck
import Text.Trifecta
import qualified Data.Map as M
import Control.Monad
import Jatko.Pretty
import System.IO

build :: FilePath -> IO ()
build path = parseFromFile parseWhole path >>= \case
  Just decs -> do
    h <- openFile "out.js" WriteMode
    forM_ (M.toList $ decVars decs) $ \(k, e) -> do
      putStr $ k ++ ": "
      r <- runTC decs $ unbound k
      putStrLn $ ppExpr decs NoPrecedence r
      when (k == "main") $
        hPutStrLn h $ "var " ++ mangle k ++ " = " ++ compile (simplify 10 decs e) ++ ";"
    {-
    forM_ (M.toList $ decRegistries decs) $ \(k, e) -> do
      hPutStrLn h $ "var " ++ mangle k ++ " = " ++ compile (simplify 10 decs $ Case e) ++ ";"
    -}
    hClose h
  Nothing -> return ()
