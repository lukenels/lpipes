
module Example where

import LPipes
import Control.Monad

-- Square Example

squareClient :: Client Int Int IO ()
squareClient = forM_ [1..10] $ \x -> do
  y <- request x
  lift $ print y

squareServer :: Int -> Server Int Int IO z
squareServer num = respond (num * num) >>= squareServer

runSquare :: IO ()
runSquare = runEffect $
  squareServer +>> squareClient

