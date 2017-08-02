
{-# LANGUAGE EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LPipes where

data Proxy a' a b' b m r =
  Pure r |
  M (m (Proxy a' a b' b m r)) |
  Request a' (a -> Proxy a' a b' b m r) |
  Respond b (b' -> Proxy a' a b' b m r)

instance (Monad m) => Functor (Proxy a' a b' b m) where
  fmap f (Pure x) = Pure $ f x
  fmap f (M a) = M $ fmap (fmap f) a
  fmap f (Request a g) = Request a $ fmap (fmap f) g
  fmap f (Respond b g) = Respond b $ fmap (fmap f) g

instance (Monad m) => Applicative (Proxy a' a b' b m) where
  pure = Pure
  (Pure f) <*> mx = fmap f mx
  (Request a cont) <*> mx = Request a $ \x -> (cont x <*> mx)
  (Respond a cont) <*> mx = Respond a $ \x -> (cont x <*> mx)
  (M mf) <*> mx = M $ mf >>= \x -> return $ x <*> mx

instance (Monad m) => Monad (Proxy a' a b' b m) where
  return = pure
  (Pure x) >>= mf = mf x
  (Request a cont) >>= mf = Request a $ \x -> (cont x >>= mf)
  (Respond a cont) >>= mf = Respond a $ \x -> (cont x >>= mf)
  (M mx) >>= mf = M $ mx >>= \x -> return $ x >>= mf

data X

-- Type aliases

type Pipe a b = Proxy () a () b
type Consumer a = Proxy () a () X
type Producer b = Proxy X () () b
type Client a' a = Proxy a' a () X
type Server b' b = Proxy X () b' b
type Effect = Proxy X () () X

-- Functions

runEffect :: (Monad m) => Effect m r -> m r
runEffect (Pure r) = return r
runEffect (M m) = m >>= runEffect
runEffect (Respond x _) = case x of
runEffect (Request x _ ) = case x of

respond :: b -> Proxy a' a b' b m b'
respond x = Respond x Pure

request :: a' -> Proxy a' a b' b m a
request x = Request x Pure

pull :: a' -> Proxy a' a a' a m r
pull x = Request x push

push :: a -> Proxy a' a a' a m r
push x = Respond x pull

lift :: (Monad m) => m x -> Proxy a' a b' b m x
lift eff = M $ fmap Pure eff

(Pure r) >>~ _ = Pure r
(Request a cont) >>~ client = Request a $ \x -> (cont x >>~ client)
(Respond a cont) >>~ client = cont +>> (client a)
(M mx) >>~ client = M $ mx >>= \x -> return (x >>~ client)

(server >~> client) x = (server x) >>~ client

_ +>> (Pure r) = Pure r
server +>> (Respond a cont) = Respond a $ \x -> (server +>> cont x)
server +>> (Request a cont) = (server a) >>~ cont
server +>> (M mx) = M $ mx >>= \x -> return (server +>> x)

(server >+> client) x = server +>> client x

func >\\ (Respond a cont) = Respond a $ \x -> (func >\\ cont x)
func >\\ (Request a cont) = func a >>= \x -> func >\\ cont x
func >\\ (M mx) = M $ mx >>= \x -> return (func >\\ x)
_ >\\ (Pure x) = Pure x

(func \>\ client) x = func >\\ (client x)

(Respond a cont) //> func = func a >>= \x -> cont x //> func
(Request a cont) //> func = Request a $ \x -> cont x //> func
(M mx) //> func = M $ mx >>= \x -> return (x //> func)
(Pure x) //> _ = Pure x

(server />/ func) x = (server x) //> func

reflect (M mx) = M $ mx >>= return . reflect
reflect (Request a cont) = Respond a $ reflect . cont
reflect (Respond a cont) = Request a $ reflect . cont
reflect (Pure x) = Pure x

-- Simpler

up >-> down = (\_ -> up) +>> down

await :: Proxy () a b' b m a
await = request ()

yield :: b -> Proxy a' a b' b m b'
yield = respond
