module ToC.Util where

join :: [String] -> String -> String
join [] _ = ""
join (x:xs) sep = x ++ foldl (\acc x' -> acc ++ sep ++ x') "" xs
