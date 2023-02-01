module Parser (
    Parser, parseChar, parseAnyChar,
    parseOr, parseAnd, parseAndWith,
    parseMany, parseSome, parseUInt, parseInt,
    ParseError (..), ParseErrorContent (..)) where

-- data ListError = ParenthesisNotClosed
--     | MissingComa
-- instance Show ListError where
--   show (ParethesisNotClose) = "Parenthesis is not closed"

type Pos = Int

data ParseErrorContent = InvalidSymbol
    | InvalidLiteral
    | InvalidList -- ListError
    | CharacterNotFound
    deriving (Eq, Show)

data ParseError = ParseError !Pos !ParseErrorContent
    deriving (Eq, Show)

type Parser a = String -> Either ParseError (a, String)


parseChar :: Char -> Parser Char
parseChar c (a:as)
  | c == a = Right (c, as)
  | otherwise = Left (ParseError 0 CharacterNotFound)
parseChar _ [] = Left (ParseError 0 CharacterNotFound)

parseAnyChar :: String -> Parser Char
parseAnyChar (c:cs) (a:as)
    | c == a = Right (c, as)
    | otherwise = parseAnyChar cs (a:as)
parseAnyChar _ _ = Left (ParseError 0 CharacterNotFound)

parseOr :: Parser a -> Parser a -> Parser a
parseOr pa pb str = case pa str of
    Left _ -> pb str
    Right s -> Right s

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd pa pb str = pa str >>= (\(a, s) -> pb s >>=
    (\(b, s2) -> Right ((a, b), s2)))

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f pa pb str = parseAnd pa pb str >>= (\((a, b), s) -> Right (f a b, s))

parseMany :: Parser a -> Parser [a]
parseMany pa str = case pa str of
    Left _ -> Right ([], str)
    Right (a, s) -> parseMany pa s >>= (\(as, s2) -> Right (a:as, s2))

parseSome :: Parser a -> Parser [a]
parseSome pa str = parseAnd pa (parseMany pa) str >>=
    (\((a, as), s) -> Right (a:as, s))

parseUInt :: Parser Int
parseUInt str = parseSome (parseAnyChar ['0'..'9']) str >>=
    (\(as, s) -> Right (read as, s))

parseInt :: Parser Int
parseInt ('-':str) = parseUInt str >>= (\(nb, s) -> Right (-nb, s))
parseInt ('+':str) = parseUInt str
parseInt str = parseUInt str

-- parsePair :: Parser a -> Parser (a, a)