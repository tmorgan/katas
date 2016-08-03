import Data.List
f x = findIndex (==x) $ [concat $ map show $ take y $ repeat y | y<-[0..length x]]

main = do
  putStrLn $ show $ f $ concat $ take 1000000 $ repeat "1000000"

