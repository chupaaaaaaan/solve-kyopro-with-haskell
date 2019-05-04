import Data.List
main :: IO ()
main = do
  _ <- getLine
  bs <- map read . words <$> getLine
  putStrLn $ case solve bs [] of
    Nothing -> "-1"
    Just x -> init $ unlines $ map show x

solve :: [Int] -> [Int] -> Maybe [Int]
solve [] ps = Just ps
solve xs ps =
  let check = filter (\(x,y) -> x==y) $ zip xs [1..]
  in if null check
     then Nothing
     else solve (map fst . delete (last check) $ zip xs [1..]) $ (:ps) . fst . last $ check
