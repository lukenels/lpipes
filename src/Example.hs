
module Example where

import LPipes
import Control.Monad

client :: Client Int Int IO ()
client = forM_ [1..10] $ \x -> do
  y <- request x
  lift $ print y

squareServer :: Int -> Server Int Int IO z
squareServer num = respond (num * num) >>= squareServer

sieve :: (Monad m) => Integer -> Pipe Integer Integer m ()
sieve num = forever $ do
  x <- await
  when (x `mod` num /= 0) $ yield x

foo :: IO ()
foo = runEffect $ squareServer +>> client
