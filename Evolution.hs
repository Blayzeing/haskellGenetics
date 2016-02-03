module Evolution where
import System.Random
import Data.List
import Debug.Trace

-- Takes a count of the number of inputs to generate, a max and a minimum
generateStartState :: Int -> Int -> Double -> Double -> [Double]
generateStartState seed size max min = take size randomNums
    where randomNums = randomRs (min,max) (mkStdGen seed) :: [Double]

mutateStateNgenes :: Int -> [Double] -> Double -> Double -> Int -> [Double]
mutateStateNgenes 0 state _ _ _ = state
mutateStateNgenes _ [] _ _ _ = []
mutateStateNgenes count state max min seed = firstSegM ++ [mutatedGene] ++ secondSegM
    where
        randomNums = take (length state) (randoms (mkStdGen seed) :: [Int])
	nextSeed = head randomNums
        highest = maximum randomNums
        pos = getMaybe $ elemIndex highest randomNums
        zipped = map (\ (s,n) -> if n == highest then (s,1) else (s,0)) (zip state randomNums)
        mutation = head (randomRs (min,max) (mkStdGen seed) :: [Double])
        mutatedGene = (foldr (\ (s,n) r -> if n == 1 then r + s else r) 0 zipped) + mutation
        firstSeg = map fst $ take pos zipped
        secondSeg = map fst $ drop (pos+1) zipped
        nextIteration = mutateStateNgenes (count-1) (firstSeg++secondSeg) max min nextSeed
        firstSegM = take (length firstSeg) nextIteration
        secondSegM = drop (length firstSeg) nextIteration
        getMaybe (Just a) = a


---- Evolution Section: All functions here run the actual evolution, handling selection and precedence
-- Recursive function to spawn 100 variations on a gene state, run them through the given function and then choose the best state and run it again, until genCount == maxGens
-- ordering can be 'compare' for lowest first, or '(flip compare)' for highest first
evolve :: Int -> [Double] -> Int -> Double -> Double -> (Double -> Double -> Ordering) -> ([Double] -> Double) -> ([Double] -> Double -> Double -> Int -> [Double]) -> Int -> Int -> [Double]
evolve seed state genSize max min ordering function mutationFunction genCount maxGens
    | genCount == maxGens = state
    | otherwise = evolve nextSeed (fst best) genSize max min ordering function mutationFunction (genCount+1) maxGens
    where
        seeds = randoms (mkStdGen seed) :: [Int]
        mutatedStates = state:[mutationFunction state max min x | x <- take genSize seeds]
        rankedMutations = map (\ state -> (state, function state)) mutatedStates
        orderedMutations@(best:t) = sortBy (\ (_,n1) (_,n2) -> ordering n1 n2) rankedMutations
        nextSeed = head $ drop genSize seeds

-- Same as above, but instead of function just taking a state, it now takes a seed and a state (in that order)
evolveSeeded :: Int -> [Double] -> Int -> Double -> Double -> (Double -> Double -> Ordering) -> (Int -> [Double] -> Double) -> ([Double] -> Double -> Double -> Int -> [Double]) -> Int -> Int -> [Double]
evolveSeeded seed state genSize max min ordering function mutationFunction genCount maxGens
    | genCount == maxGens = state
    | otherwise = traceShow (state) $ evolveSeeded nextSeed (fst best) genSize max min ordering function mutationFunction (genCount+1) maxGens
    where
        seeds = randoms (mkStdGen seed) :: [Int]
        mutatedStates = (seed, state):[(x, mutationFunction state max min x) | x <- take genSize seeds]
        rankedMutations = map (\ (seed, state) -> (state, function seed state)) mutatedStates
        orderedMutations@(best:t) = sortBy (\ (_,n1) (_,n2) -> ordering n1 n2) rankedMutations
        nextSeed = head $ drop genSize seeds
