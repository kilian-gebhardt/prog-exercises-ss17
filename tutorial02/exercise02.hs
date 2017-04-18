import Prelude hiding (words, unwords)

words :: String -> [String]
words inp = go "" inp

go :: [Char] -> String -> [String]
go w "" = [w]
go w (' ':cs) = w : go "" cs
go w (c:cs)   = go (w ++ [c]) cs

unwords :: [String] -> String
unwords [] = ""
unwords [w] = w
unwords (w:ws) = w ++ " " ++ unwords ws 
