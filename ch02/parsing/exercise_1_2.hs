module Main where
import Control.Monad (liftM)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
     args <- getArgs
     putStrLn (readExpr (args !! 0))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
            char '"'
            s <- many (noneOf "\"")
            char '"'
            return $ String s

parseAtom :: Parser LispVal
parseAtom = do
          first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          let atom = first:rest
          return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= (\x -> return ((Number . read) x))


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
         Left err -> "No match: " ++ show err
         Right val -> "Found value"
