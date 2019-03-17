module Parser (parseDpll) where
  import qualified Data.Text as T
  import Data.Char (digitToInt)
  import IntDpll (Clause, Literal)
  
  parseDpll :: [Char] -> Maybe [Clause]
  parseDpll = (parseDpll' [] []) . dpllex
  
  parseDpll' :: [Clause] -> Clause -> [Token] -> (Maybe [Clause])
  parseDpll' _    _    (Error _:_)     = Nothing
  parseDpll' cacc lacc (EOF:xs)        = Just (lacc : cacc)
  parseDpll' cacc lacc (Newline:xs)    = parseDpll' (lacc:cacc) []       xs
  parseDpll' cacc lacc (Literal x: xs) = parseDpll' cacc        (x:lacc) xs
 
  
  data Token = Literal Int | Newline | EOF | Error [Char] deriving (Show, Eq)
  dpllex :: [Char] -> [Token]
  dpllex [] = [EOF]
  dpllex (x:xs)
    | x == 'c'                = dpllex (skipUntilNewline xs)
    | x == '\n'               = Newline:(dpllex xs)
    | x `elem` '-':['0'..'9'] = num : dpllex rest
    | x `elem` " \t"          = dpllex xs
    | otherwise               = [Error ("Unexpected char: " ++ [x])]
    where (num, rest) = parseInt (x:xs)

    

  skipUntilNewline :: [Char] -> [Char]
  skipUntilNewline [] = []
  skipUntilNewline (x:xs)
    | x == '\n' = xs
    | otherwise = skipUntilNewline xs
    
  parseInt :: [Char] -> (Token, [Char])
  parseInt = parseInt' 0 1 
  parseInt' acc sign [] = (Literal acc, []) 
  parseInt' acc sign (x:xs) 
    | x == '-'            = if acc == 0 && (head xs) `elem` ['0'..'9']
                            then parseInt' acc (-1) xs 
                            else (Error "Illegal '-' in number literal", xs)
    | x `elem` ['0'..'9'] = parseInt' ((abs acc)*10*sign + sign*digitToInt x) sign xs
    | otherwise           = (Literal acc, x:xs)