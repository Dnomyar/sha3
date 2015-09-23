


module Utils (
    showHexa
    , showBinary
    , xorReduceLine
    , xorLine
    , rotateList
    , rotateMat
    , notB
    ) where

    import Numeric (showHex, showIntAtBase)
    import Data.Char (intToDigit)

    import Data.Bits

    showHexa :: (Integral a, Show a) => a -> String
    showHexa number = "0x" ++ showHex number ""

    showBinary :: (Integral a, Show a) => a -> String
    showBinary number = "0b" ++ showIntAtBase 2 intToDigit number ""

    xorReduceLine :: [Int] -> Int
    xorReduceLine = foldl1 (\a b -> xor a b) 

    xorLine :: [Int] -> [Int] -> [Int]
    xorLine lineA lineB = map (\(a,b) -> xor a b) $ zip lineA lineB

    -- http://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
    rotateList :: Int -> [a] -> [a]
    rotateList _ [] = []
    rotateList n xs = zipWith const (drop n (cycle xs)) xs


    rotateMat :: Int -> [[Int]] -> [[Int]] 
    rotateMat size a = map (\(i, line) -> rotateList i line) $ zip [0..size] a 


    notB :: Int -> Int
    notB a = ((-1) *) $ complement a 