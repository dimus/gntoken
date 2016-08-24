module Token
    ( Token
    , verbatim
    , cleaned
    , start
    , end
    , tokenize
    ) where

import Data.Char

data Token = Token { verbatim :: String
                   , cleaned :: String
                   , start :: Int
                   , end :: Int
                   , nextSpace :: Maybe Char
                   } deriving (Show, Eq)


tokenize :: String -> Int -> [Token]
tokenize s o =
  let
    tokens = preTokenize s o
  in
    removeDivs tokens


--private

removeDivs :: [Token] -> [Token]
removeDivs [] = []
removeDivs (x:[]) = x:[]
removeDivs (x:y:xs) =
  if detectDiv x y
    then (mergeDiv x y) : removeDivs xs
    else x : removeDivs (y:xs)


mergeDiv :: Token -> Token -> Token
mergeDiv t1 t2 =
  Token ((verbatim t1) ++ (verbatim t2)) ((cleaned t1) ++ (cleaned t2)) (start t1) (end t2) (nextSpace t2)


detectDiv :: Token -> Token -> Bool
detectDiv t1 t2
  | (length . verbatim $ t1) < 2 = False
  | not $ endOfLine  t1 t2 = False
  | not . isLower . head .verbatim $ t2 = False
  | z /= '-' = False
  | not . isLower $ y = False
  | otherwise = True
  where
    (z:y:ax) = reverse . verbatim $ t1


endOfLine :: Token -> Token -> Bool
endOfLine t1 t2
  | (((`elem` "\r\n") <$> (nextSpace t1)) == (Just True)) = True
  | start t2 - end t1 > 1 = True
  | otherwise = False


preTokenize :: String -> Int -> [Token]
preTokenize [] _ = []
preTokenize s o =
  let (t, ts) = break (`elem` " \n\r") s
      end = o + (length t)
      token = Token t (flipStrip t) o end (spaceChar ts)
      tokens = tokenize (restOf ts) (end + 1)
  in
    if null t
      then tokens
      else token : tokens


spaceChar :: [Char] -> Maybe Char
spaceChar []    = Nothing
spaceChar (x:_) = Just x


restOf :: [Char] -> [Char]
restOf [] = []
restOf (_:xs) = xs


flipStrip :: String -> String
flipStrip x =
  let f = dropWhile (not . isAlphaNum) . reverse
  in f . f $ x
