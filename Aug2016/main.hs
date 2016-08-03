import Control.Monad
import Data.List
import Data.Ord

data Exp = M Exp Exp | D Exp Exp | S Exp Exp | A Exp Exp | Val Int deriving (Eq, Ord)

instance Show Exp where
  show (M a b) = "("++(show a) ++ "*" ++ (show b) ++")"
  show (D a b) = "("++(show a) ++ "/" ++ (show b) ++")"
  show (S a b) = "("++(show a) ++ "-" ++ (show b) ++")"
  show (A a b) = "("++(show a) ++ "+" ++ (show b) ++")"
  show (Val a) = (show a)

eval (M a b) = (eval a) * (eval b) 
eval (D a b) | (eval b) == 0 = 0
             | otherwise = (eval a) `div` (eval b) 
eval (S a b) = (eval a) - (eval b) 
eval (A a b) = (eval a) + (eval b) 
eval (Val a) = a 

powerset = filterM (const [True, False])

doit :: [Int] -> [Exp] -> [Exp]
doit [] acc =  acc
doit [x] acc = [ Val x ] ++ acc
doit (x:xs) acc = [ (M a b) | a <- doit [x] acc, b <- doit xs acc ] ++
                  [ (D b a) | a <- doit [x] acc, b <- doit xs acc ] ++ 
                  [ (S a b) | a <- doit [x] acc, b <- doit xs acc ] ++
                  [ (A a b) | a <- doit [x] acc, b <- doit xs acc ] 

x = concat $ map permutations $ powerset [100,75,50,25,6,3]
trees = concat $ map (\x -> doit x []) x 
attempts = map eval trees
target = 952
res = minimumBy (comparing snd) $ map (\(a,b) -> (a, abs(b - target))) (zip trees attempts)
main = print $ fst $ res 

