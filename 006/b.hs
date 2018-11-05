import Data.Array

main :: IO ()
main = do
  n <- (read <$> getLine) :: IO Int
  let f | n == 1 = array (0,0) ((0,0):[]) :: Array Int Int
        | n == 2 = array (0,1) ((0,0):(1,0):[]) :: Array Int Int
        | n == 3 = array (0,2) ((0,0):(1,0):(2,1):[]) :: Array Int Int
        | otherwise = array (0,n-1) ((0,0):(1,0):(2,1):[(k, (f!(k-3)+f!(k-2)+f!(k-1)) `mod` 10007) | k<-[3..n-1]]) :: Array Int Int
  putStrLn $ show $ (f!(n-1))
