module Evolution where
import System.Random
import Data.List
import Debug.Trace

-- Takes a count of the number of inputs to generate, a max and a minimum
generateStartState :: Int -> Int -> Double -> Double -> [Double]
generateStartState seed size max min = take size randomNums
    where randomNums = randomRs (min,max) (mkStdGen seed) :: [Double]


---- Mutators Section: All functions here take a state, and mutate it in some way. If you write any more (or improve on my hastely-written code), please send me your additions
-- Takes seed, a state, and the maximum and minimum mutation amount, then returns the state with 1 randomly mutated gene
mutateStateOneGene :: [Double] -> Double -> Double -> Int -> [Double]
mutateStateOneGene state max min seed = map (\ (stateBit, rNum) -> if rNum == highest then stateBit + mutation else stateBit) zipped
    where
        randomNums = randoms (mkStdGen seed) :: [Int]
        sequence = take (length state) randomNums
        zipped = zip state (sequence)
        highest = maximum sequence
        doubleRandoms = randomRs (min,max) (mkStdGen seed) :: [Double]
        mutation = head (drop (length state) doubleRandoms)

-- Same as above, just with 2 randomly mutated genes
mutateStateTwoGenes :: [Double] -> Double -> Double -> Int -> [Double]
mutateStateTwoGenes state max min seed = map fst $ map (mutate second mutation2) $ map (mutate highest mutation) zipped
    where
        randomNums = randoms (mkStdGen seed) :: [Int]
        sequence = take (length state) randomNums
        zipped = zip state (sequence)
        highest = maximum sequence
        second = maximum (delete highest sequence)
        mutate bit mut (stateBit, rNum) = if rNum == bit then (stateBit + mut, rNum) else (stateBit, rNum)
        doubleRandoms = randomRs (min,max) (mkStdGen seed) :: [Double]
        mutation = head (drop (length state) doubleRandoms)
        mutation2 = head (drop ((length state)+1) doubleRandoms)

mutateStateNgenes :: [Double] -> Double -> Double -> Int -> Int -> [Double]
mutateStateNgenes state _ _ _ 0 = state
mutateStateNgenes [] _ _ _ _ = []
mutateStateNgenes state max min seed count = firstSeg ++ [mutatedGene] ++ secondSeg--(mutateStateNgenes ) ++ mutatedGene:(mutateStateNgenes )
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
