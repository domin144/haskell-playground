{-# LANGUAGE InstanceSigs #-}

module StateAndMaybe
  ( StateMaybe (StateMaybe),
    getStateMaybe,
    MaybeWithState (MaybeWithState),
    getMaybeWithState,
  )
where

import Control.Monad.State (State)

newtype StateMaybe s a = StateMaybe {getStateMaybe :: State s (Maybe a)}

instance Functor (StateMaybe s) where
  fmap :: (a -> b) -> StateMaybe s a -> StateMaybe s b
  fmap f (StateMaybe x) = StateMaybe $ fmap (fmap f) x

instance Applicative (StateMaybe s) where
  pure :: a -> StateMaybe s a
  pure = StateMaybe . pure . pure
  (<*>) :: StateMaybe s (a -> b) -> StateMaybe s a -> StateMaybe s b
  StateMaybe f <*> StateMaybe x = StateMaybe $ do
    maybeFF <- f
    case maybeFF of
      Just fF -> fmap fF <$> x
      Nothing -> pure Nothing

-- The line below produces behavior incompatible with later monad instance
-- fmap (<*>) f <*> x

instance Monad (StateMaybe s) where
  (>>=) :: StateMaybe s a -> (a -> StateMaybe s b) -> StateMaybe s b
  StateMaybe h >>= f = StateMaybe $ do
    maybeX <- h
    case maybeX of
      Nothing -> return Nothing
      Just x -> getStateMaybe (f x)

newtype MaybeWithState s a = MaybeWithState
  { getMaybeWithState :: s -> Maybe (a, s)
  }

instance Functor (MaybeWithState s) where
  fmap :: (a -> b) -> MaybeWithState s a -> MaybeWithState s b
  fmap f (MaybeWithState x) =
    MaybeWithState $ fmap fOnFirst . x
    where
      fOnFirst (y, s) = (f y, s)

instance Applicative (MaybeWithState s) where
  pure :: a -> MaybeWithState s a
  pure x = MaybeWithState $ \s -> Just (x, s)
  (<*>) :: MaybeWithState s (a -> b) -> MaybeWithState s a -> MaybeWithState s b
  MaybeWithState f <*> x = MaybeWithState $ \s ->
    do
      (ff, newS) <- f s
      getMaybeWithState (fmap ff x) newS

instance Monad (MaybeWithState s) where
  return :: a -> MaybeWithState s a
  return x = MaybeWithState $ \s -> return (x, s)
  (>>=) :: MaybeWithState s a -> (a -> MaybeWithState s b) -> MaybeWithState s b
  MaybeWithState h >>= f = MaybeWithState $ \s ->
    do
      (x, newS) <- h s
      getMaybeWithState (f x) newS