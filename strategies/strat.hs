module Strat where

import Control.DeepSeq
import Control.Parallel.Strategies

parPairs :: Strategy (a,b)
parPairs (x,y) = do
  x' <- rpar x
  y' <- rpar y
  return (x',y')

{-
 - Paramaterized Strategies
 - Baking in parallel evaluation into structures can take, as
 - inhabitants in the structure, themselves structures which
 - have parallelism baked in.
 -}

parArgPair :: Strategy a -> Strategy b -> Strategy (a,b)
parArgPair f g (x,y) = do
  x' <- f x
  y' <- g y
  return (x',y')

{-
 - Using the general parArgPair we can choose how we want
 - to evaluate our structures, we can even forgo lazy evaluation in parallel!
 -}

forceEval :: NFData a => Strategy a
forceEval x = rpar (force x)

deepPair :: NFData a => NFData b => Strategy (a,b)
deepPair (x,y) = parArgPair forceEval forceEval (x,y)

with :: Strategy a -> Strategy a -> Strategy a
with f g x = do x' <- f x
                g x'

parWith :: Strategy a -> Strategy a
parWith strat = with strat rpar

{-
 - Evaluating lists in parallel
 - Map parallel evaluation over every value in the list
 -}

parFmap :: (a -> b) -> [a] -> [b]
parFmap f xs = using (map f xs) (parLists rseq)

parLists :: Strategy a -> Strategy [a]
parLists = evalLists . parWith

evalLists :: Strategy a -> Strategy [a]
evalLists _ [] = return []
evalLists f (x:xs) = do
  x'  <- f x
  xs' <- parLists f xs
  return (x': xs')


