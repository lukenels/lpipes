
module Example where

import LPipes
import Control.Monad

client :: Proxy Int Int x y IO ()
client = forM_ [1..10] $ \x -> do
  y <- request x
  lift $ print y

squareServer :: Int -> Proxy x y Int Int IO z
squareServer num = respond (num * num) >>= squareServer

foo :: IO ()
foo = runEffect $ squareServer +>> client
