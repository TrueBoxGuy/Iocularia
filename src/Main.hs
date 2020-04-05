module Main where

import Lambda.Lambda
import Transpiler.Conversion
import qualified Lambda.Parser as L
import qualified Transpiler.Parser as T
import Control.Monad.Except (runExceptT, liftEither, ExceptT(..), liftIO)
import Paths_Lambda_Calculus

-- | The final program, which calls @'runTranspiler'@ and @'runLambdaCalc'@ in order.
main :: IO ()
main = runExceptT (runTranspiler *> runLambdaCalc) >>= either putStrLn mempty

-- | Reads from input.il, transpiles the code, and writes it to input.la.
runTranspiler :: ExceptT String IO ()
runTranspiler
  = T.input >>= liftEither . toExpression
  >>= liftIO . (getDataFileName "input.la" >>=) . flip writeFile . show

-- | Simplifies the code in input.la and outputs it to output.la.
runLambdaCalc :: ExceptT String IO ()
runLambdaCalc
  = L.input
  >>= liftIO . (getDataFileName "output.la" >>=) . flip writeFile . show . reduce 0 (-1)
