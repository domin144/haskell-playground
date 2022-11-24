import TestStateAndMaybe (testStateAndMaybe)

main :: IO ()
main = do
  result <- testStateAndMaybe True
  if result then return () else error "fail"
