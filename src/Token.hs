module Token
    ( Token
    , verbatim
    , cleaned
    , start
    , end
    , zeroToken
    , tokenize
    , slice
    ) where

import Data.Char

type Offset = Int
data Token = Token { verbatim :: String
                   , cleaned :: String
                   , start :: Int
                   , end :: Int
                   , nextSpace :: Maybe Char
                   } deriving (Show, Eq)

tokenize :: Token -> String -> [Token]
tokenize t [] = t : []
tokenize t1 s =
  let (w, rest) = break (`elem` " \n\r") s
      start = 1 + end t1
      t2 = tokenInit w start rest
  in
    if mergeable t1 t2
      then tokenize (merge t1 t2) (stripSpace rest)
      else t1 : tokenize t2 (stripSpace rest)


zeroToken = tokenInit [] (-1) []


tokenInit :: String -> Offset -> String -> Token
tokenInit w o rest =
  Token w (flipStrip w) o (o + length w) (spaceChar rest)

slice :: String -> Token -> String
slice s t =
  take len . drop (start t) $ s
  where
    len = (end t) - (start t)

--private

merge :: Token -> Token -> Token
merge t1 t2 =
  if (null . verbatim $ t1) then t2
  else Token ((verbatim t1) ++ (verbatim t2)) ((cleaned t1) ++
             (cleaned t2)) (start t1) (end t2) (nextSpace t2)


mergeable :: Token -> Token -> Bool
mergeable t1 t2
  | null . verbatim $ t1 = True
  | null . verbatim $ t2 = True
  | otherwise = detectDiv t1 t2


detectDiv :: Token -> Token -> Bool
detectDiv t1 t2
  | (length . verbatim $ t1) < 2 = False
  | not $ endOfLine  t1 t2 = False
  | not . isLower . head . verbatim $ t2 = False
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


spaceChar :: [Char] -> Maybe Char
spaceChar []    = Nothing
spaceChar (x:_) = Just x


stripSpace :: [Char] -> [Char]
stripSpace [] = []
stripSpace (_:xs) = xs


flipStrip :: String -> String
flipStrip x =
  let f = dropWhile (not . isAlphaNum) . reverse
  in f . f $ x
