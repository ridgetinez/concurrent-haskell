module Kmeans where

import Control.Parallel.Strategies
import Control.Deepseq

data Point = Point Double Double

computeCentroid :: [Point] -> Int -> Point -> Point
computeCentroid points d oldCentroid = Point centroidX centroidY
    where pointsForCentroid = filter (\p -> dist p oldCentroid <= d) points
          centroidPair = foldr (\(Point x y) (x', y') -> (x+x', y+y')) (0,0) pointsForCentroid
          centroidX = (fst centroidPair) / (length points)
          centroidY = (snd centroidPair) / (length points)

dist :: Point -> Point -> Double
dist (Point x1, y1) (Point x2, y2) = sqrt $ (square $ x1 - x2) + (square $ y1 - y2)

clusters :: Int -> Int -> (Int, Int) -> [Point] -> [Point]
clusters iters numClusters dims points =
    findClusterCentroids iters numClusters points (startingCentroids dims numClusters)

startingCentroids :: (Int, Int) -> Int -> [Point]
startingCentroids (x,y) numClusters = error "not implemented"

findClusterCentroids :: Int -> Int -> [Point] -> [Point] -> [Point]
findClusterCentroids iters numClusters points acc
    | iters == 0 = acc
    | otherwise = findClusterCentroids (iters-1) points (map (computeCentroid points d) acc)

