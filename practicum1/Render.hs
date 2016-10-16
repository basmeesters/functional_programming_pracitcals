-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Render where

import Types
import Parser
import SVG

-- A function in which the 'op' expects a min/max function and the 'xory' a fst/ snd function and returns the smallest/ biggest of the x or y respectively
getDimensions :: (Float -> Float -> Float) -> ((Float, Float) -> Float) -> Track -> Float
getDimensions op xory (x, y, z) = foldr op (xory $ fst $ head z) (map (\(i,j) -> op (xory i) (xory j)) z)

-- Using the getDimensions for the minimum and maximum x and y coordinates of the track
calculateDimensions :: Track -> Dimensions
calculateDimensions (x,y,z) = ((floor(getDimensions min fst (x,y,z))), 
                               (floor(getDimensions min snd (x,y,z))), 
                               (ceiling(getDimensions max fst (x,y,z))), 
                               (ceiling(getDimensions max snd (x,y,z))))

-- Converts the given list of traces into a list of lines which the renderline expects
convertTraces :: [Trace] -> [Line]
convertTraces [] = []
convertTraces (x:xs) = traceToLines x ++ convertTraces xs

-- Converts one trace into a list of lines
traceToLines :: Trace -> [Line]
traceToLines [] = []
traceToLines (x:[]) = []
traceToLines (x:y:ys) = [(x, y)] ++ traceToLines (y:ys)

-- Using the renderLine on the boundary lines of the track and the traces and the linesToSvg to write SVG output to a file
raceToSvg :: FilePath -> Track -> [Trace] -> IO()
raceToSvg fp (x, y, z) xs = linesToSvg  fp (calculateDimensions (x, y, z)) ((map (renderLine "Black") z) ++ [renderLine "Green" y] ++ (map (renderLine "Red") (convertTraces xs)))