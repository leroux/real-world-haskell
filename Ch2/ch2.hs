lastButOne :: [a] -> a
lastButOne [] = error "lastButOne: empty list"
lastButOne [x, _] = x
lastButOne (_:xs) = lastButOne xs
