{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser (Parser, satisfy, pChar, pChars, pString, pWhitespace, parseOr, parseAnd, parseAndWith, parseMany, parseSome, pUInt, pInt, pParenthesis, pBool, parsePair, parseList) where
import Control.Applicative ( Alternative(empty, (<|>), many, some) )
import Data.List ( nub )

data ParseError = InvalidSynthax
  | Unexpected
  deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String ->  Either [ParseError] (a, String)
}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left [InvalidSynthax]

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \i ->
  case i of 
    [] -> Left [InvalidSynthax]
    x:xs
      | f x -> Right (x,xs)
      | otherwise -> Left [InvalidSynthax]

pChar :: Char -> Parser Char
pChar h = satisfy (== h)

pChars :: String -> Parser Char
pChars s = satisfy (`elem` s)

pString :: String -> Parser String
pString = traverse pChar  

pWhitespace :: Parser Char
pWhitespace = satisfy (`elem` " \n\t") 

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b = a <|> b

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd a b = (,) <$> a <*> b

parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith f a b = f <$> a <*> b

parseMany :: Parser a -> Parser [ a ]
parseMany = many 

parseSome :: Parser a -> Parser [ a ]
parseSome = some

pUInt :: Parser Int
pUInt = read <$> some (pChars ['0'..'9'])

pInt :: Parser Int
pInt = parseAndWith (*) (pChar '-' *> pure (-1)) pUInt

pParenthesis :: Parser a -> Parser a
pParenthesis p = pChar '(' *> p <* pChar ')'

pBool :: Parser Bool
pBool = parseOr (pString "true" *> pure True) (pString "false" *> pure False)

parsePair :: Parser a -> Parser (a, a)
parsePair p = pParenthesis $ parseAndWith (,) p (pChar ',' *> p)

parseList :: Parser a -> Parser [a]
parseList p = pParenthesis $ parseAndWith (:) p (parseMany (pChar ',' *> p))
