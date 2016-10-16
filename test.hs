module Test where

import Data.Tree

split [] = []
split (x:xs) = (x,xs) : [(y,x:zs) | (y,zs) <- split xs]
permutations [] = [[]]
permutations l = [h:p | (h,t) <- split l, p <- permutations t]

ndsl [] = [[]]
ndsl (x:xs) = ndsl' x (x:xs)
ndsl' _ [] = [[]]
ndsl' h (x:xs) | h <= x = map (x:) (ndsl' x xs) ++ ndsl' h xs
               | otherwise = ndsl' h xs
               

enumBf :: Tree a -> [a]
enumBf t = enumBf' [t]
    where enumBf' ts = (enumBf.concat([[l,r] | Node l r <- ts]))