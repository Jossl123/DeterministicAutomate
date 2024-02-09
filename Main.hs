module Main where
import Automate

automateMultiplesOfTen :: Automate
automateMultiplesOfTen = createAutomate alphabet [a, b] a [b]
    where
        a = createNode "a" [createTransition "0" 1, createTransition "1" 0, createTransition "2" 0, createTransition "3" 0, createTransition "4" 0, createTransition "5" 0, createTransition "6" 0, createTransition "7" 0, createTransition "8" 0, createTransition "9" 0] 
        b = createNode "b" [createTransition "0" 1, createTransition "1" 0, createTransition "2" 0, createTransition "3" 0, createTransition "4" 0, createTransition "5" 0, createTransition "6" 0, createTransition "7" 0, createTransition "8" 0, createTransition "9" 0]
        alphabet = createAlphabet [[c] | c <- ['0' .. '9']]

automateEvenNumbers :: Automate
automateEvenNumbers = createAutomate alphabet [a, b] a [b]
    where
        a = createNode "a" [createTransition "1" 0,createTransition "3" 0,createTransition "5" 0,createTransition "7" 0,createTransition "9" 0, createTransition "0" 1, createTransition "2" 1, createTransition "4" 1, createTransition "6" 1, createTransition "8" 1]
        b = createNode "b" [createTransition "1" 0,createTransition "3" 0,createTransition "5" 0,createTransition "7" 0,createTransition "9" 0, createTransition "0" 1, createTransition "2" 1, createTransition "4" 1, createTransition "6" 1, createTransition "8" 1]
        alphabet = createAlphabet [[c] | c <- ['0' .. '9']]

main :: IO ()
main = do
    let automate = automateMultiplesOfTen
    print automate
    print $ executeAutomate automate "10"
    print $ executeAutomate automate "509"
