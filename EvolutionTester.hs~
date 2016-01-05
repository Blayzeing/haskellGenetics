module EvolutionTester where
import Evolution

-- Simple optimisation problem:
problemFunction :: [Double] -> Double
problemFunction inputs = sum (map (\ (input, target) -> abs(target-input)) zipped)
    where zipped = zip inputs [4,1,2]

exampleProblemFunction :: [Double] -> [Double] -> Double
exampleProblemFunction measure inputs = sum (map (\ (input, target) -> abs(target-input)) zipped)
    where zipped = zip inputs measure

