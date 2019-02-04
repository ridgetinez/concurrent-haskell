module ParallelKMeans where

import Control.Parallel.Strategies

data Point = Point Double Double
data PointSum = PointSum Int Double Double
data Cluster = Cluster { clId :: Int
                       , clCen :: Point
                       }

zeroPoint :: Point
zeroPoint = Point 0 0

sqDist :: Point -> Point -> Double
sqDist (Point x1 y1) (Point x2 y2) = ((x1 - x2)^2) + ((y1 - y2)^2)

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum n xs ys) (Point x y) = PointSum (n+1) (xs+x) (ys+y)

toPoint :: PointSum -> Point
toPoint (PointSum n xs ys) = Point (xs / fromIntegral n) (ys / fromIntegral n)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster id psum = Cluster { clId = id
                                    , clCen = toPoint psum
                                    }

-- clusters is non empty!
assign :: Int -> [Point] -> [Cluster] -> Vector PointSum
assign nclusters points clusters = Vector.create $ do
    vec <- MVector.replicate (length clusters) (PointSum 0 0 0)
    let addPoint point = do
        let cid = nearest point
        ps <- MVector.read vec cid
        MVector.write vec cid $! addToPointSum ps p
    in
    mapM_ addPoint point
    return vec
    where
        nearest p = let centroids = map (\c -> c.clCen) clusters
            in foldr (\(i,c) (d,i') -> if sqDist c p < d then (sqDist c p, i) else (d,i'))
                     (sqDist p (centroids !! 0), 0)
                     centroids





