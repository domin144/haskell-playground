module TestStateAndMaybe (testStateAndMaybe) where

import Control.Monad.State (State, runState, state)
import StateAndMaybe (StateMaybe (StateMaybe), getStateMaybe)

testStateMaybe :: Bool
testStateMaybe =
  run (m1 <*> m2) == run (m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2))))
  where
    m1 = StateMaybe $ state $ \s -> (Nothing, s + 1)
    m2 = StateMaybe $ state $ \s -> (Just 1, s + 1)
    run :: StateMaybe Integer Integer -> (Maybe Integer, Integer)
    run m = runState (getStateMaybe m) 0

testStateAndMaybe :: Bool -> IO Bool
testStateAndMaybe result = do
  putStrLn "TestStateAndMaybe"
  putStrLn $ "testStateMaybe = " ++ show testStateMaybe
  return (result && testStateMaybe)