module Token
    ( Token()
    , tokenize
    ) where

data Token = Token { verbatim :: String
                   , normalized :: String
                   , start :: Int
                   , end :: Int }

tokenize :: String -> [Token]

