module Evolution where
import System.Random
import Data.List
import Debug.Trace

-- Takes a count of the number of inputs to generate, a max and a minimum
generateStartState :: Int -> Int -> Double -> Double -> [Double]
generateStartState seed size max min = take size randomNums
    where randomNums = randomRs (min,max) (mkStdGen seed) :: [Double]

-- Mutates a state by changing N randomly selected genes 
mutateStateNgenes :: [Double] -> Double -> Double -> Int -> Int -> [Double]
mutateStateNgenes state _ _ _ 0 = state
mutateStateNgenes [] _ _ _ _ = []
mutateStateNgenes state max min seed count = (mutateStateNgenes ) ++ mutatedGene:(mutateStateNgenes )
    where
        randomNums = take (length state) (randoms (mkStdGen seed) :: [Int])
        zipped = zip state randomNums
        highest = 
        mutatedGene = 


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
