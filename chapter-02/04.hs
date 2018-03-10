last1 xs = head (reverse xs)

last2 xs = xs !! (length xs - 1)

last3 xs = head (drop (length xs - 1) xs)

last4 (x:[]) = x
last4 (x:xs) = last4 xs

last5 xs = take 1 (reverse xs)
