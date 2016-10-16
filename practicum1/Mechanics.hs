-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Mechanics where

import Types

crashes :: CarState -> Position -> Track -> Bool
crashes car pos (x, y, z) = or $ map (intersects (fst car, pos)) z

intersects :: Line -> Line -> Bool
intersects ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = if (ua >= 0) && (ua <= 1) && (ub >= 0) && (ub <= 1) then True else False
    where
        ua = (((x4 - x3) * (y1 - y3)) - ((y4 - y3) * (x1 - x3))) / u
        ub = (((x2 - x1) * (y1 - y3)) - ((y2 - y1) * (x1 - x3))) / u
        u = ((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1))

completes :: CarState -> Position -> Track -> Bool
completes car pos (x, y, z) = intersects (fst car, pos) y

moveValid :: CarState -> Position -> Bool
moveValid car pos = if dvx <= 1 && dvy <= 1 then True else False
    where
        dx = (fst $ fst car) - (fst pos)
        dy = (snd $ fst car) - (snd pos)
        dvx = abs ((fst $ snd car) + dx)
        dvy = abs ((snd $ snd car) + dy)