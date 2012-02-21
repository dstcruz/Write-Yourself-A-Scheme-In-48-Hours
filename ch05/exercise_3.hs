{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Char (toLower)
import Control.Monad (liftM)
import Control.Monad.Error
import Data.Array (Array (..), listArray)
import Data.Ratio (Rational (..), (%))
import Data.Complex (Complex (..))
import Numeric (readOct, readHex)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | String String
             | Char Char
             | Bool Bool
             | Vector (Array Int LispVal)

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | ExpectCondClauses
               | ExpectCaseClauses
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

main :: IO ()
main = do
   args <- getArgs
   putStrLn $ repl' (args !! 0)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
         Left err -> throwError $ Parser err
         Right val -> return val

-- not sure I understand the type of this function
-- according to ghci
repl :: IO b
repl = fmap repl' getLine >>= putStrLn >> repl

repl' :: String -> String
repl' s = extractValue . trapError $ fmap show (readExpr s >>= eval)

-- to play in ghci
p = (putStrLn . repl')

--
-- LispVal Parsers
--

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> try parseChar
          <|> try parseComplex
          <|> try parseFloat
          <|> try parseRatio
          <|> try parseNumber
          <|> parseBool
          <|> parseQuoted
          <|> parseQuasiquote
          <|> try parseUnquoteSplicing
          <|> parseUnquote
          <|> parseList

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               (return . Atom) (first:rest)

parseList :: Parser LispVal
parseList = char '(' >> parseList1

parseList1 :: Parser LispVal
parseList1 = (char ')' >> (return . List) []) 
               <|> do expr <- parseExpr
                      parseList2 [expr]

parseList2 :: [LispVal] -> Parser LispVal
parseList2 expr = (char ')' >> (return . List) (reverse expr)) 
                    <|> (spaces >> parseList3 expr)

parseList3 :: [LispVal] -> Parser LispVal
parseList3 expr = do char '.' >> spaces
                     dotted <- parseExpr
                     char ')'
                     return $ DottedList expr dotted
                  <|> do next <- parseExpr
                         parseList2 (next:expr)

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber

parsePlainNumber :: Parser LispVal
parsePlainNumber = many1 digit >>= return . Number . read

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >> 
                   (
                        parseDecimal 
                        <|> parseBinary
                        <|> parseOctal
                        <|> parseHex
                   )

parseDecimal :: Parser LispVal
parseDecimal = do char 'd'
                  n <- many1 digit
                  (return . Number . read) n

parseBinary :: Parser LispVal
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2int) n

parseOctal :: Parser LispVal
parseOctal = do char 'o'
                n <- many $ oneOf "01234567"
                (return . Number . (readWith readOct)) n

parseHex :: Parser LispVal
parseHex = do char 'x'
              n <- many $ oneOf "0123456789abcdefABCDEF"
              (return . Number . (readWith readHex)) n

parseRatio :: Parser LispVal
parseRatio = do num <- fmap read $ many1 digit
                char '/'
                denom <- fmap read $ many1 digit
                (return . Ratio) (num % denom)

parseFloat :: Parser LispVal
parseFloat = do whole <- many1 digit
                char '.'
                decimal <- many1 digit
                return $ Float (read (whole++"."++decimal))

parseComplex :: Parser LispVal
parseComplex = do r <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char '+'
                  i <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char 'i'
                  (return . Complex) (r :+ i)
               where toDouble (Float x) = x
                     toDouble (Number x) = fromIntegral x

parseString :: Parser LispVal
parseString = do char '"'
                 s <- many (escapedChars <|> (noneOf ['\\', '"']))
                 char '"'
                 (return . String) s

parseChar :: Parser LispVal
parseChar = do string "#\\"
               s <- many1 letter
               return $ case (map toLower s) of
                      "space" -> Char ' '
                      "newline" -> Char '\n'
                      [x] -> Char x

parseBool :: Parser LispVal
parseBool = do char '#'
               c <- oneOf "tf"
               return $ case c of
                      't' -> Bool True
                      'f' -> Bool False

parseQuasiquote :: Parser LispVal
parseQuasiquote = do char '`'
                     expr <- parseExpr
                     return $ List [Atom "quasiquote", expr]

-- Bug: this allows the unquote to appear outside of a quasiquoted list
parseUnquote :: Parser LispVal
parseUnquote = do char ','
                  expr <- parseExpr
                  return $ List [Atom "unquote", expr]

-- Bug: this allows unquote-splicing to appear outside of a quasiquoted list
parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do string ",@"
                          expr <- parseExpr
                          return $ List [Atom "unquote-splicing", expr]

parseVector :: Parser LispVal
parseVector = do string "#("
                 elems <- sepBy parseExpr spaces
                 char ')'
                 return $ Vector (listArray (0, (length elems)-1) elems)

--
-- Show functions
--

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

showError :: LispError -> String
showError ExpectCondClauses = "Expect at least 1 true cond clause"
showError ExpectCaseClauses = "Expect at least 1 true case clause"
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func

showError (NumArgs expected found) = "Expected " ++ show expected
                                   ++ " args; found values: " ++ unwordsList found

showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                   ++ ", found " ++ show found

showError (Parser parseErr) = "Parser error at " ++ show parseErr

--
-- Evaluator
--

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val

eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        Bool True -> eval conseq
        otherwise  -> throwError $ TypeMismatch "boolean" result

eval (List (Atom "cond" : [])) = throwError ExpectCondClauses
eval (List (Atom "cond" : cs)) = evalConds cs

eval (List (Atom "case" : [])) = throwError ExpectCaseClauses
eval (List (Atom "case" : key : cs)) = do
    keyVal <- eval key
    evalCaseCases keyVal cs

eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--
-- Evaluator Helpers
--

evalConds :: [LispVal] -> ThrowsError LispVal
evalConds (List (Atom "else" : xs) : []) = evalCondElse xs
evalConds [] = throwError ExpectCondClauses
evalConds (List clause : cs) = evalCondClause clause cs
evalConds badClauses = throwError $ TypeMismatch "cond clauses" $ List badClauses

evalCondClause (test : xs) rest = do
    result <- eval test
    case test of
         Bool False -> evalConds rest
         Bool True -> trueDo xs
         otherwise -> throwError $ TypeMismatch "boolean" result
  where 
    trueDo [] = return $ Bool True
    trueDo xs = evalToLast xs

evalCondElse :: [LispVal] -> ThrowsError LispVal
evalCondElse [] = throwError ExpectCondClauses
evalCondElse xs = evalToLast xs

evalCaseCases :: LispVal -> [LispVal] -> ThrowsError LispVal
evalCaseCases _ [] = throwError ExpectCaseClauses
evalCaseCases _ [List (Atom "else" : cExprs)] = evalToLast cExprs
evalCaseCases key ((List ((List cKeys) : cExprs)) : cs) = do
    let result = any anyOf $ map (\x -> eqv [key, x]) cKeys
    case result of
        False -> evalCaseCases key cs
        True -> evalToLast cExprs
  where
    anyOf (Right (Bool True)) = True
    anyOf _ = False
evalCaseCases _ _ = throwError ExpectCaseClauses

evalToLast :: [LispVal] -> ThrowsError LispVal
evalToLast [] = throwError $ NumArgs 1 []
evalToLast xs = liftM last $ mapM eval xs

--
-- Primitive functions lookup table
--
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)
             ,("=", numBoolBinop (==))
             ,("<", numBoolBinop (<))
             ,(">", numBoolBinop (>))
             ,("/=", numBoolBinop (/=))
             ,(">=", numBoolBinop (>=))
             ,("<=", numBoolBinop (<=))
             ,("&&", boolBoolBinop (&&))
             ,("||", boolBoolBinop (||))
             ,("string=?", strBoolBinop (==))
             ,("string<?", strBoolBinop (<))
             ,("string>?", strBoolBinop (>))
             ,("string<=?", strBoolBinop (<=))
             ,("string>=?", strBoolBinop (>=))
             ,("not", unaryOp not')
             ,("boolean?", unaryOp boolP)
             ,("list?", unaryOp listP)
             ,("symbol?", unaryOp symbolP)
             ,("char?", unaryOp charP)
             ,("string?", unaryOp stringP)
             ,("vector?", unaryOp vectorP)
             ,("symbol->string", unaryOp symbol2string)
             ,("string->symbol", unaryOp string2symbol)
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("eqv?", eqv)
             ,("eq?", eqv)
             ,("equal?", equal)
             ]

--
-- Unary primitive defs all have type
-- LispVal -> LispVal
--

not' (Bool x) = (Bool . not) x
not' _ = Bool False

boolP (Bool _) = Bool True
boolP _ = Bool False

listP (List _) = Bool True
listP (DottedList _ _) = Bool True
listP _ = Bool False

symbolP (Atom _) = Bool True
symbolP _ = Bool False

charP (Char _) = Bool True
charP _ = Bool False

stringP (String _) = Bool True
stringP _ = Bool False

vectorP (Vector _) = Bool True
vectorP _ = Bool False

symbol2string (Atom s) = String s
symbol2string _ = error "Expecting an Atom"

string2symbol (String s) = Atom s
string2symbol _ = error "Expecting a String"

--
-- Other primitives
--

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [x] y] = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x,y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool b1), (Bool b2)] = (return . Bool) $ b1 == b2
eqv [(Number n1), (Number n2)] = (return . Bool) $ n1 == n2
eqv [(String s1), (String s2)] = (return . Bool) $ s1 == s2
eqv [(Atom a1), (Atom a2)] = (return . Bool) $ a1 == a2

eqv [(DottedList xs x), (DottedList ys y)] = 
    eqv [List $ xs ++ [x], List $ ys ++ [y]]

eqv [(List l1), (List l2)] = (return . Bool) $ all byPairs $ zip l1 l2
  where byPairs (x,y) = case eqv [x,y] of
                             Left err -> False
                             Right (Bool val) -> val

eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [(List l1), (List l2)] = (return . Bool) $ all byPairs $ zip l1 l2
  where byPairs (x,y) = case equal [x,y] of
                             Left err -> False
                             Right (Bool val) -> val

equal [(DottedList xs x), (DottedList ys y)] =
    equal [List $ xs ++ [x], List $ ys ++ [y]]

equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList

--
-- Primitive helpers
--

numericBinop :: (Integer -> Integer -> Integer) 
             -> [LispVal] 
             -> ThrowsError LispVal
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unaryOp :: (LispVal -> LispVal) 
        -> [LispVal] 
        -> ThrowsError LispVal
unaryOp func [arg] = return $ func arg

boolBinop :: (LispVal -> ThrowsError a) 
          -> (a -> a -> Bool) 
          -> [LispVal] 
          -> ThrowsError LispVal
boolBinop unpacker op [x,y] = do
    left <- unpacker x
    right <- unpacker y
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

--
-- Misc Helpers
--

escapedChars :: Parser Char
escapedChars = do
             char '\\'
             c <- oneOf ['\\','"', 'n', 'r', 't']
             return $ case c of
                    '\\' -> c
                    '"'  -> c
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space
bin2int :: String -> Integer
bin2int s = sum $ map (\(i,x) -> i*(2^x)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1

readWith :: (t -> [(a, b)]) -> t -> a
readWith f s = fst $ f s !! 0

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
