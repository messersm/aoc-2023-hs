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
natural :: ReadP Int
natural = read <$> (many1 $ satisfy isDigit)

newline :: ReadP Char
newline = char '\n'

space :: ReadP Char
space = char ' '

-- | Runs the parser `p` and ignores the result.
skip :: Applicative f => f a -> f ()
skip p = p *> pure ()
