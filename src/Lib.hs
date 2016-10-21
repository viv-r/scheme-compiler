module Lib where

import           Control.Monad
import           Text.ParserCombinators.Parsec hiding (spaces)

run :: String -> String
run = show . eval . readExpr

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> parseParenthesis

parseParenthesis :: Parser LispVal
parseParenthesis = do
  _ <- char '('
  x <- try parseList <|> parseDottedList
  _ <- char ')'
  return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  headList <- endBy parseExpr spaces
  tailList <- char '.' >> spaces >> parseExpr
  return $ DottedList headList tailList

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "No match: " ++ show err
  Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "string: " ++ contents
showVal (Atom name) = "atom: " ++ name
showVal (Number num) = "number: " ++ show num
showVal (Bool True) = "true: #t"
showVal (Bool False) = "false: #f"
showVal (List a) = "list: " ++ show a
showVal (DottedList a b) = "dotted-list with " ++ show b ++ ": " ++ show a

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
