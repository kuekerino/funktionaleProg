flatten_r :: [[a]] -> [a]
flatten_r xss = foldr (++) [] xss

flatten_l :: [[a]] -> [a]
flatten_l xss = foldl (++) [] xss

flatten_list :: [[a]] -> [a]
flatten_list [] = []
flatten_list xss = [ x | y <- xss,
                         x <- y]
