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

-- clusters is non empty, and clId is the index of the cluster within clusters
-- | A Haskell Vector is a data structure that allows user to program as if you could mutate a collection of data.
-- | Here we create a vector of PointSums where index i is the PointSum for the ith element in the clusters list.
-- | For each point, we add the (x,y) coords of the point to the closest centroid and finally return the vector as immutable
-- | (Vector.create provides an interface to update the vector, when done the vector is no longer able to be updated)
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
        nearest p = let centroids = [(c.clId, sqDist p c.clCen) | c <- clusters]
            in fst $ minimumBy (\s t -> (snd s) `compare` (snd t)) centroids

-- | Must handle possibility that numPoints to centroid is 0
-- | Difference between map and comprehension: map retains structure (length of xs pre and post is the same)
-- | Comprehensions do not have that property enforced
makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vs = map (uncurry pointSumToCluster) (zipWith [0..(length nonEmptyPointSums)] (nonEmptyPointSums))
    where nonEmptyPointSums = filter (\(PointSum n _ _) -> n /= 0) (toList vs)

-- | Update cluster positions once more
step :: Int -> [Point] -> [Cluster] -> [Cluster]
step nclusters points clusters = makeNewClusters (assign nclusters points clusters)

maxSequentialIters = 80

-- | TODO: Lift into IO monad to print cluster state per iteration
sequentialKMeans :: Int -> [Point] -> [Cluster] -> [Cluster]
sequentialKMeans nclusters points clusters = let
    loop :: Int -> [Cluster] -> [Cluster]
    loop iter clusters
        | maxSequentialIters <= iter = clusters
        | otherwise = let clusters' = step nclusters points clusters in
            if clusters' == clusters
                then clusters'
                else loop (iter+1) clusters'
    in loop 0 clusters

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

