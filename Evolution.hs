module Evolution where
import System.Random
import Data.List
import Debug.Trace

-- Takes a count of the number of inputs to generate, a max and a minimum
generateStartState :: Int -> Int -> Double -> Double -> [Double]
generateStartState seed size max min = take size randomNums
    where randomNums = randomRs (min,max) (mkStdGen seed) :: [Double]

---- MUTATION FUNCTIONS BELOW HERE
mutateStateNgenes :: Int -> Double -> Double -> [Double] -> Int -> [Double]
mutateStateNgenes 0 _ _ state _ = state
mutateStateNgenes _ _ _ [] _ = []
mutateStateNgenes count max min state seed = firstSegM ++ [mutatedGene] ++ secondSegM
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
        nextIteration = mutateStateNgenes (count-1) max min (firstSeg++secondSeg) nextSeed
        firstSegM = take (length firstSeg) nextIteration
        secondSegM = drop (length firstSeg) nextIteration
        getMaybe (Just a) = a

-- EXAMPLE USAGE OF EVOLVE FUNCTION:
-- evolve [seed] [initial state] [generation size] [ordering func] [scoring func] [       mutation func      ] 0 [total # of generations]
-- evolve   42      [0,0,0]             10         (flip.compare)     chessSim    (mutateStateNgenes 2 1 (-1)) 0          100

---- Evolution Section: All functions here run the actual evolution, handling selection and precedence
-- Recursive function to spawn 100 variations on a gene state, run them through the given function and then choose the best state and run it again, until genCount == maxGens
-- ordering can be 'compare' for lowest first, or '(flip compare)' for highest first
evolve :: Int -> [Double] -> Int -> (Double -> Double -> Ordering) -> ([Double] -> Double) -> ([Double] -> Int -> [Double]) -> Int -> Int -> [Double]
evolve seed state genSize ordering function mutationFunction genCount maxGens
    | genCount == maxGens = traceShow (function state) $ state
    | otherwise = traceShow (state) $ evolve nextSeed (fst best) genSize ordering function mutationFunction (genCount+1) maxGens
    where
        seeds = randoms (mkStdGen seed) :: [Int]
        mutatedStates = state:[mutationFunction state x | x <- take genSize seeds]
        rankedMutations = map (\ state -> (state, function state)) mutatedStates
        orderedMutations@(best:t) = sortBy (\ (_,n1) (_,n2) -> ordering n1 n2) rankedMutations
        nextSeed = head $ drop genSize seeds

-- EXAMPLE USAGE OF EVOLVESEEDED FUNCTION:
-- evolve [seed] [initial state] [generation size] [ordering func] [scoring func] [       mutation func      ] 0 [total # of generations]
-- evolve   42      [0,0,0]             10         (flip.compare)   ArandomChess  (mutateStateNgenes 2 1 (-1)) 0          100

-- Same as above, but instead of function just taking a state, it now takes a seed and a state (in that order)
evolveSeeded :: Int -> [Double] -> Int -> (Double -> Double -> Ordering) -> (Int -> [Double] -> Double) -> ([Double] -> Int -> [Double]) -> Int -> Int -> [Double]
evolveSeeded seed state genSize ordering function mutationFunction genCount maxGens
    | genCount == maxGens = state
    | otherwise = traceShow (state) $ evolveSeeded nextSeed (fst best) genSize ordering function mutationFunction (genCount+1) maxGens
    where
        seeds = randoms (mkStdGen seed) :: [Int]
        mutatedStates = (seed, state):[(x, mutationFunction state x) | x <- take genSize seeds]
        rankedMutations = map (\ (seed, state) -> (state, function seed state)) mutatedStates
        orderedMutations@(best:t) = sortBy (\ (_,n1) (_,n2) -> ordering n1 n2) rankedMutations
        nextSeed = head $ drop genSize seeds
