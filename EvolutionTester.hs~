module EvolutionTester where
import Evolution

-- Simple optimisation problem:
problemFunction :: [Double] -> [Double] -> Double
problemFunction measure inputs = sum (map (\ (input, target) -> abs(target-input)) zipped)
    where zipped = zip inputs measure

-- Example use of the optimisation function to optimise to [4,1,2]
exampleProblemFunction :: [Double] -> Double
exampleProblemFunction = problemFunction [4,1,2]
