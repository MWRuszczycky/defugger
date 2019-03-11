
import Control.Applicative              ( Alternative
                                        , empty
                                        , (<|>)         )
import Types

---------------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ s -> do (s', x) <- p s
                                           Just (s', f x)

instance Applicative Parser where
    pure x                  = Parser $ \ s -> Just (s, x)
    Parser pl <*> Parser pr = Parser $ \ s -> do (s1, f) <- pl s
                                                 (s2, x) <- pr s1
                                                 Just (s2, f x)


instance Alternative Parser where
    empty                   = Parser $ \ s -> Nothing
    Parser pl <|> Parser pr = Parser $ \ s -> pl s <|> pr s

instance Monad Parser where
    Parser p >>= g = Parser $ \ s -> do (s1, x) <- p s
                                        runParser (g x) s1

