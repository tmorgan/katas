import Data.List
import Tests

f x = 1 + (length $ filter (== '\n') $ dropWhile (/= '*') x)
buildings x =  filter (\x -> all not (map (all (== ' ')) x) || length x < 2) $ groupBy (\x y -> (all id $ map (== ' ') x) == (all id $ map (== ' ') y)) $ map reverse $ transpose $ lines x
b x = 1 + (length $ takeWhile (\x -> not $ any id (map ((any id).(map (== '*'))) x)) (buildings x))
a x = 1 + (length $ takeWhile (/= '*') $ filter (/= ' ') $ head $ dropWhile (not.(any id).(map (== '*'))) $ concat $ map transpose (buildings x))

roomNumber x = show (b x) ++ show (f x) ++ show (a x)
test (Test input output) = roomNumber input == output

main = putStrLn $ show $ map test tests
