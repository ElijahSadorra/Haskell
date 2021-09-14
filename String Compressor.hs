import Data.Char 

chomp :: String -> String
chomp []     = []
chomp (x:xs) = takeWhile (== x) (x:xs)

munch :: String -> String
munch = take 9 . chomp

runs :: String -> [String]
runs [] = []
runs xs = ys : runs (drop (length ys) xs) 
          where ys = munch xs 
 
encode :: String -> [(Char,Int)]
encode xs = [(head z,length z) | z<-runs xs]

flatten :: [(Char, Int)] -> String
flatten ((x,y):zs) = [x] ++ show y ++ flatten zs
flatten _          = [] 

compress :: String -> String
compress = flatten . encode

decode :: [(Char, Int)] -> String
decode ((x,y):zs) = replicate y x ++ decode zs 
decode _         = ""

expand :: String -> [(Char, Int)]
expand (x:y:zs) = [(x,digitToInt y)] ++ expand zs
expand _        = []

decompress :: String -> String
decompress = decode . expand

