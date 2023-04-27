module Main where

import Text.ParserCombinators.ReadP
import qualified Data.Char as C
import Data.Composition

data Expr a = Number a 
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Sub (Expr a) (Expr a) deriving (Show)

type BinaryOp a = Expr a -> Expr a -> Expr a

skipTrailingSpaces :: ReadP a -> ReadP a
skipTrailingSpaces a = do
  result <- a
  _ <- skipSpaces
  return result

binaryOps :: [String] -> [BinaryOp a] -> ReadP (BinaryOp a)
binaryOps = (skipTrailingSpaces . foldl1 (+++)) .: zipWith toBinaryOp
  where
    toBinaryOp x f = string x >> return f

parse :: Read a => String -> Expr a
parse = success . last . readP_to_S init_expr
  where
    success (result, "") = result
    paren_parse = between (char '(') (char ')') $ do parse_expr

    init_expr = do skipSpaces; parse_expr
    term = skipTrailingSpaces (paren_parse <++ number)
    parse_div_mul = term `chainl1` binaryOps ["*", "/"] [Mul, Div]
    parse_expr = parse_div_mul `chainl1` binaryOps ["+", "-"] [Add, Sub]
    number = Number . read <$> munch1 C.isDigit

eval :: Fractional a => Expr a -> a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b
eval (Number n) = n

main :: IO ()
main = do
    x <- getLine
    print . eval $ (parse x :: Expr Double)