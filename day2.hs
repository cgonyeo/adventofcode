import Data.List.Split
import Data.List

part1 :: [String] -> Int
part1 [] = 0
part1 (x:xs) = l*w + (2*l*w + 2*w*h + 2*h*l) + part1 xs
    where [l,w,h] = parseLine x

part2 :: [String] -> Int
part2 [] = 0
part2 (x:xs) = 2*(l+w) + (l*w*h) + part2 xs
    where [l,w,h] = parseLine x

parseLine :: String -> [Int]
parseLine = sort . map read . splitOn "x"
