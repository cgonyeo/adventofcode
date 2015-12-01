part1 :: String -> Int
part1 xs = part1Helper xs 0

part1Helper :: String -> Int -> Int
part1Helper ('(':xs) count = part1Helper xs (count+1)
part1Helper (')':xs) count = part1Helper xs (count-1)
part1Helper []       count = count


part2 :: String -> Int
part2 xs = part2Helper xs 0 0

part2Helper :: String -> Int -> Int -> Int
part2Helper _        count (-1)  = count
part2Helper ('(':xs) count floor = part2Helper xs (count+1) (floor+1)
part2Helper (')':xs) count floor = part2Helper xs (count+1) (floor-1)
