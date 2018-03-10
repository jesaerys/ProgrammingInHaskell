init1 xs = reverse (tail (reverse xs))

init2 xs = take (length xs - 1) xs

init3 (x:[]) = []
init3 (x:xs) = [x] ++ init3 xs

init4 xs = reverse (drop 1 (reverse xs))
