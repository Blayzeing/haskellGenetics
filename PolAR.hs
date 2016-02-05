module PolAR where
import Evolution
import Debug.Trace

-- Simulation of the PolAR Goggles, using various batteries
-- Maximises Power, minimises weight, power must be over x mAh
-- RPi2 uses ~350mA @ 5.2V
-- 2 1080p cameras (RPi cams) - 2x ~250mA @ Above voltage
-- blutooth dongle ~30mA
-- Total: 880mA

-- Batteries:
-- iPhone 4 similar - 1420mAh - 100g
-- microsoft lumia  - 1569mAh - 100g
-- aricona N*473    - 2200mAh - 70g  lithium battery
-- aricona          - 2600mAh - 100g
-- aricona          - 3000mAh - 100g
-- EPCTEK power bank-20000mAh - 299g
polarSim :: [Double] -> Double
polarSim counts
    | tW + baseWeight> 300 = 0
    | otherwise = traceShow(tW) $ tPwr
    where
        -- Pi + camera x2 + goggle weight (Bolle ski goggles 125g)
        baseWeight = 45.0 + 5.0 + 5.0 + 125.0 + 5.0
        powerData = [1420.0,1569.0,2200.0,2600.0,3000.0]
        weightData = [100.0,100.0,70.0,100.0,100.0]
        (tPwr, tW) = foldr (\ (cP,cW) (rP,rW) -> (rP+cP, rW+cW)) (0,0) $ map (score baseWeight) (zip3 powerData weightData counts)

score :: Double -> (Double, Double, Double) -> (Double, Double)
score base (pwr, weight, counts) = (totalPwr, totalWeight)
    where
        totalWeight = weight * counts
        totalPwr = pwr * counts
