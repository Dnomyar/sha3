
module Main where

import Data.Bits
import Data.List


import Utils



theta :: [[Int]] -> [[Int]]
theta aInput = 
    let c = map xorReduceLine aInput in -- c :: [Int]
    let d = theta_d c in -- d :: [Int]
    transpose $ map (\lineA -> xorLine lineA d) $ transpose aInput

theta_d :: [Int] -> [Int]
theta_d c = map (\(x,y) -> xor x y) $ zip listRotateRightOne listRotateLeftOne where 
    listRotateRightOne = rotateList 4 c
    listRotateLeftOne  = map (\el -> shiftL el 1) (rotateList 1 c)





rho_pi :: [[Int]] -> [[Int]] -> [[Int]] 
rho_pi a r = rotateMat4 $ map (\(lineAt, lineR) -> rho_pi_line lineAt lineR) $ zip (transpose a) r 
    where
        rotateMat4 = rotateMat 4

rho_pi_line :: [Int] -> [Int] -> [Int]
rho_pi_line lat lr = map (\(eat, er) -> shiftL eat er) $ zip lat lr




chi :: [[Int]] -> [[Int]]
chi b = transpose $ map manageLine $ transpose b
    where
        manageLine line = map (\(lineR0, (lineR1, lineR2)) -> xor lineR0 ((notB lineR1) .&. lineR2)) $ zip line $ zip (rotateList 1 line) (rotateList 2 line)


iota :: [[Int]] -> Int -> [[Int]]
iota ((x : xs) : xss) rc = ((xor x rc : xs) : xss)

main = putStrLn $ show $ rotateMat 4 [[0..4],[0..4],[0..4],[0..4],[0..4]]