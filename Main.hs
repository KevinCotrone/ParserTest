module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error

data LispVal = Atom String
			 |List [LispVal]
			 |DottedList [LispVal] LispVal
			 |Number Integer
			 |String String
			 |Bool Bool

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
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


main :: IO ()
main = do
	args <- getArgs	
	evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
	putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val



eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError action = catchError action (return . show)

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args : found values " 
																			++ unwordsList found
showError (Parser err) = "Parse error at " ++ show err
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
						($ args)
						(lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
			  ("-", numericBinop (-)),
			  ("*", numericBinop (*)),
			  ("/", numericBinop div),
			  ("mod", numericBinop mod),
			  ("quotient", numericBinop quot),
			  ("remainder", numericBinop rem),
			  ("=", numBoolBinop (==)),
		      ("<", numBoolBinop ( <)),
			  (">", numBoolBinop ( >) ),
			  ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop ( >)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /=2
							 then throwError $ NumArgs 2 args
							 else do 
								left <- unpacker $ args !! 0
								right <- unpacker $ args !! 1
								return $ Bool $ left `op` right

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
						if null parsed
						  then throwError $ TypeMismatch "number" $ String n
						  else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number"  notNum

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList start end) = "(" ++ unwordsList start ++ " . " ++ showVal end ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


parseExpr :: Parser LispVal
parseExpr = parseAtom
		<|> parseString
		<|> parseNumber
		<|> parseQuoted
		<|> parseList

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
parseQuoted = do
	char '\''
	expr <- parseExpr
	return $ List [ Atom "quote", expr]

parseDottedList :: Parser LispVal
parseDottedList = do
	start <- endBy parseExpr spaces
	end <- char '.' >> spaces >> parseExpr
	return $ DottedList start end


parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many $ many1 (noneOf "\"\\") <|> escapedChars
	char '"'
	return $ String (concat x)

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = [first] ++ rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_ -> Atom atom

escapedChars :: Parser String
escapedChars = do 
	char '\\'
	x <- oneOf "\\\""
	return [x]

spaces :: Parser ()
spaces = skipMany1 space