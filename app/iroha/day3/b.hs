import Control.Monad.Trans.Maybe

main :: IO ()
main = do


query :: (Int,Int,Int) -> IO ()
query (x,y,z) = putStrLn $ "? " ++ show x ++ " " ++ show y ++ " " ++ show z

answer :: IO (String, String)
answer = (\[x,y] -> (x,y)) . words <$> getLine

-- solve :: IO ()
-- solve = do

