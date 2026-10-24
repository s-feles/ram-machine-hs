module Zipper where

type Zipper a = ([a], [a])

moveLeft :: Zipper a -> Zipper a
moveLeft ([], _) = error "No item to left of cursor"
moveLeft (x:xs, ys) = (xs, x:ys)

moveRight :: Zipper a -> Zipper a
moveRight (_, []) = error "No item to right of cursor"
moveRight (xs, y:ys) = (y:xs, ys)

getItem :: Zipper a -> Maybe a
getItem (_, x:_) = Just x
getItem (_, []) = Nothing

place :: a -> Zipper a -> Zipper a
place a (l, []) = (l, [a])
place a (l, _:r) = (l, a:r)
