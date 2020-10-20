-- |

module LearnParsers where

import           Text.Trifecta
import           Control.Monad.State
import           Control.Monad
import           Control.Applicative

{-
look the similarity:
        type Parser a = String -> Maybe (a, String)
        newtype Reader a = Reader {runReader :: r -> a}
        newtype State s a = State { runState :: s -> (a, s) }

(>>) :: Monad m => m a -> m b -> mb
-}

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser b
one' = char '1' >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'
oneTwo' :: Parser b
oneTwo' = oneTwo >> stop -- commenting this out will generate error

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"
{-
Exercise parsing practice
-}
oneEof :: Parser Char
oneEof = one <* eof

oneTwoEof :: Parser Char
oneTwoEof = oneTwo <* eof


{-
End Excersize
-}


pNL :: String -> IO ()
pNL s = putStrLn (('\n' : s))

main2401 :: IO ()
main2401 = do
  pNL "stop:"
  testParse stop
  pNL "one: "
  testParse one
  pNL "one': "
  testParse one'
  pNL "oneTwo: "
  testParse oneTwo
  pNL "oneTwo': "
  testParse oneTwo'
  pNL "oneEof: "
  testParse oneEof

