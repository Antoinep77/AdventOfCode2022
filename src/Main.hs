module Main where


import System.IO  
import Control.Monad

import Day1.Exo1


main :: IO ()
main = do  
        let list = []
        handle <- openFile "src/Day1/input.txt" ReadMode
        contents <- hGetContents handle
        let response = solution contents
        print response
        hClose handle   
