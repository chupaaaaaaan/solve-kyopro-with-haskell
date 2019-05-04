import qualified Data.Set as S
main :: IO ()
main = do
  s <- S.fromList <$> getLine :: IO (S.Set Char)
  let a = filter (\x -> fst x) . map (\x -> (,) ((not . S.member x) s) x) $ ['a'..'z']
  putStrLn $ if null a
             then "None"
             else [snd . head $ a]
