{-# LANGUAGE InstanceSigs #-}
module StateAndMaybe (StateMaybe (StateMaybe), getStateMaybe) where

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

-- newtype MaybeWithState s a = MaybeWithState
--   { getMaybeWithState :: s -> Maybe (a, s)
--   }

-- instance Monad (MaybeWithState s) where
--   return x = MaybeWithState $ \s -> return (x, s)
--   h >>= f = MaybeWithState $ \s ->
--     do
--       (x, newS) <- getMaybeWithState h s
--       getMaybeWithState (f x) newS