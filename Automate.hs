module Automate(createAlphabet, createNode, createTransition, createAutomate, executeAutomate, Automate) where 

-- ALPHABET

newtype Alphabet = Alphabet [String]
instance Show Alphabet where
    show (Alphabet alphabet) = show alphabet

-- create alphabet
createAlphabet :: [String] -> Alphabet
createAlphabet = Alphabet



-- TRANSITION

data Transition = Transition String Int
instance Eq Transition where
    (Transition input1 output1) == (Transition input2 output2) = input1 == input2 && output1 == output2
instance Show Transition where
    show (Transition input output) = "t:" ++ show input ++ "->" ++ show output

createTransition :: String -> Int -> Transition
createTransition = Transition

checkTransition :: Transition -> String -> Bool
checkTransition (Transition l n) w = l == w


-- NODE

data Node = Node String [Transition]
instance Eq Node where
    (Node name1 transitions1) == (Node name2 transitions2) = name1 == name2 && transitions1 == transitions2
instance Show Node where
    show (Node name transitions) = show name ++ show transitions

-- Create a node with the given transition function
createNode :: String -> [Transition] -> Node
createNode = Node

--Get the output node from an input String 
getOutputNodeIndex :: Node -> String -> Int
getOutputNodeIndex (Node _ transitions) input = getOutputNodeIndex' transitions input
    where
        getOutputNodeIndex' :: [Transition] -> String -> Int
        getOutputNodeIndex' [] _ = error "No transition found"
        getOutputNodeIndex' (transition@(Transition _ output):transitions) input = if checkTransition transition input then output else getOutputNodeIndex' transitions input


-- AUTOMATE

data Automate = Automate {alphabet:: Alphabet, startState:: Node, finalStates:: [Node], nodes::[Node]}
instance Show Automate where
    show (Automate _ _ _ nodes) = show nodes

-- Create a new Automate with the given nodes
createAutomate :: Alphabet -> [Node] -> Node -> [Node] -> Automate
createAutomate alphabet nodes startState finalStates = Automate alphabet startState finalStates nodes 

executeAutomate :: Automate -> String -> Bool
executeAutomate (Automate _ startState finalStates nodes) = executeAutomate' startState
    where
        executeAutomate' :: Node -> String -> Bool
        executeAutomate' node [] = isMember node finalStates
        executeAutomate' node (x:xs) = executeAutomate' (nodes !! getOutputNodeIndex node [x]) xs

-- Checks if a node is a final state and print the node for debugging
isMember :: Node -> [Node] -> Bool
isMember _ [] = False
isMember node (x:xs) = node == x || isMember node xs

main :: IO ()
main = do
    let a = createNode "a" [Transition "1" 0,Transition "2" 0, Transition "3" 0, Transition "4" 0, Transition "5" 0, Transition "6" 0, Transition "7" 0, Transition "8" 0, Transition "9" 0, Transition "0" 1]
    let b = createNode "b" [Transition "1" 0,Transition "2" 0, Transition "3" 0, Transition "4" 0, Transition "5" 0, Transition "6" 0, Transition "7" 0, Transition "8" 0, Transition "9" 0, Transition "0" 1]
    let automate = createAutomate (createAlphabet ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]) [a, b] a [b]
    let word = "0" 
    print $ executeAutomate automate word
    let word = "1" 
    print $ executeAutomate automate word