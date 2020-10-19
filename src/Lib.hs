module Lib
    ( AppState(..)
      , WriterT(..)
      , Reader(..)
      , divParsedXY
    ) where

import Text.Read (readMaybe)

----------------------------------
newtype Writer w a = Writer { runWriter :: (a, w) }
----------------------------------

instance Functor (Writer w) where
  fmap f (Writer (a1, w1)) = Writer (f a1, w1)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  (Writer (f, w1)) <*> (Writer (x, w2)) =
    Writer (f x, w1 `mappend` w2)

instance Monoid w => Monad (Writer w) where
  (Writer (a1, w1)) >>= f =
    let (Writer (a2, w2)) = f a1
    in (Writer (a2, w1 `mappend` w2))

----------------------------------

tell :: Monoid w => w -> Writer w ()
tell x = Writer ((), x)

----------------------------------
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
----------------------------------

instance Monad m => Functor (WriterT w m) where
  fmap f (WriterT g) = WriterT $ do
    (a1, w1) <- g
    return (f a1, w1)

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
  pure x = WriterT $ return (x, mempty)
  (WriterT g) <*> (WriterT h) = WriterT $ do
    (f, w1) <- g
    (x, w2) <- h
    return (f x, w1 `mappend` w2)

instance (Monad m, Monoid w) => Monad (WriterT w m) where
  (WriterT g) >>= f = WriterT $ do
    (a1, w1) <- g
    let (WriterT h) = f a1
    (a2, w2) <- h
    return (a2, w1 `mappend` w2)

----------------------------------

initWriterT :: (Monad m, Monoid w) => m a -> WriterT w m a
initWriterT g =
  let pairWithEmpty a = (a, mempty)
  in WriterT $ fmap pairWithEmpty g

tTell :: (Monad m, Monoid w) => w -> WriterT w m ()
tTell x = WriterT $ return ((), x)

lTell :: Monad m => a -> WriterT [a] m ()
lTell x = tTell [x]

----------------------------------

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ \i -> f (g i)

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x
  (Reader g) <*> (Reader h) = Reader $ \i ->
    let f = g i
        x = h i
    in f x

instance Monad (Reader r) where
  (Reader g) >>= f = Reader $ \i ->
    let a = g i
        h = runReader $ f a
        a' = h i
    in a'

----------------------------------

ask :: Reader r r
ask = Reader $ \i -> i

asks :: (a -> b) -> Reader a b
asks f = Reader $ \i -> f i

----------------------------------

--parseInts :: String -> WriterT [] m [Int]

data AppState = AppState { getX :: String, getY :: String }

divParsedXY :: WriterT [String] (Reader AppState) Int
divParsedXY = do
  parsedX <- initWriterT $ asks (readMaybe . getX)
  x <- case parsedX of
    Nothing -> do
      x <- initWriterT $ asks getX
      lTell $ "Couldn't parse x: " ++ x
      return 0
    Just v -> return v
  parsedY <- initWriterT $ asks (readMaybe . getY)
  y <- case parsedY of
    Nothing -> do
      y <- initWriterT $ asks getY
      lTell $ "Couldn't parse y: " ++ y
      return 0
    Just v -> return v
  if y == 0
    then do
      lTell $ "Division by 0. Returning default value"
      return 0
    else return $ x `div` y
