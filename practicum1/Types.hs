-- Bas Meesters, 3700569
-- Thijs B. Klaver, 3711633

module Types where

type Point = (Float, Float)
type Line = (Point, Point)
type Velocity = (Float, Float)
type Position = (Float, Float)
type CarState = (Position, Velocity)
type Trace = [Position]
type Track = (Point, Line, [Line])