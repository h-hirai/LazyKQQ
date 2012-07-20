-- Lazy K interpreter, original written by @fumieval
-- http://ideone.com/Blsr4

import Control.Arrow
import Control.Applicative
import Data.Char (chr, ord)
import System.Exit
import System.Environment
import System.IO

infixl 9 :$
data Expr = Expr :$ Expr|I|K|S|Inc|Export {-# UNPACK #-} !Int deriving Show

apply :: Expr -> Expr -> Expr
apply (S :$ x :$ y) z = apply x z `apply` apply y z
apply (K :$ x) y = x
apply I x = x
apply Inc (Export x) = Export $! x + 1
apply Inc _ = error "attempted to apply inc to a non-number"
apply f x = f :$ x

eval :: Expr -> Expr
eval (x :$ y) = eval x `apply` eval y
eval x = x

cons :: Expr -> Expr -> Expr
cons a b = S :$ (S :$ I :$ (K :$ a)) :$ (K :$ b)

church :: Int -> Expr
church 0 = K :$ I
church 1 = I
church 256 = S :$ I :$ I :$ (S :$ I :$ I :$ (S :$ (S :$ (K :$ S) :$ K) :$ I))
church n = S:$ (S :$ (K :$ S) :$ K) :$ church (n - 1)


encode :: String -> Expr
encode = foldr cons (cons (church 256) (church 256)) . map (church . ord)

export :: Expr -> Int
export (Export x) = x
export x = error $ "invalid output format (result was not a number:" ++ show x

realize :: Expr -> Int
realize expr = export $ expr `apply` Inc `apply` Export 0

parse :: String -> (Expr, String)
parse ('`':xs) = let (a0, xs') = parse xs
                     (a1, xs'') = parse xs' in
                 (a0 :$ a1, xs'')
parse ('s':xs) = (S, xs)
parse ('k':xs) = (K, xs)
parse ('i':xs) = (I, xs)
parse (_:xs) = parse xs
parse "" = (I, "")

output :: Expr -> IO ()
output expr
    | x < 256 = putChar (chr x) >> (output $ apply expr $ K :$ I)
    | x == 256 = exitWith ExitSuccess
    | otherwise = exitWith $ ExitFailure $ x - 256
    where
      x = realize $ apply expr K

main = do
  prog <- readFile "program.lazyk"
  input <- getContents
  let p = fst $ parse $ concat $ filter ((/='#').head) $ filter (/=[]) $ lines prog
      i = encode input
  output $ eval (p :$ i)
