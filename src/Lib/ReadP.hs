module Lib.ReadP where

import Data.Char
import Text.ParserCombinators.ReadP

-- | ReadP parser for non-negative integers
--
-- Examples:
--
-- >>> readP_to_S (natural <* eof) "411"
-- [(411,"")]
-- >>> readP_to_S (natural <* eof) "0"
-- [(0,"")]
-- >>> readP_to_S (natural <* eof) "sometext0"
-- []
-- >>> readP_to_S (natural <* eof) "-20"
-- []
natural :: (Integral a, Read a) => ReadP a
natural = read <$> (many1 $ satisfy isDigit)

newline :: ReadP Char
newline = char '\n'

space :: ReadP Char
space = char ' '

-- | Runs the parser `p` and ignores the result.
skip :: Applicative f => f a -> f ()
skip p = p *> pure ()

-- | Parse pairs of `p` and `q`
--
-- Example:
--
-- >>> readP_to_S ((pair natural natural) `sepBy` (many1 space) <* eof) "79 14 55 13"
-- [([(79,14),(55,13)],"")]
pair :: ReadP a -> ReadP b -> ReadP (a, b)
pair p q = do
  x <- p
  space *> skipSpaces
  y <- q
  return (x, y)
