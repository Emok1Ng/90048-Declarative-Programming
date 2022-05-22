--  File     : Proj2.hs
--  Author   : Wenjun Wang 1249890 www4@student.unimelb.edu.au
--  Purpose  : Related funtions to support guessing the three locations in the
--  4x8 grid in limited time and steps. (within 6 guesses and 5 second)
--
--  Description: The game is to guess three target locations in a 4x8 grid, each
--  time the distance between the target and the guess will be given. By analyzing
--  the feedback, this program first removes the possible target that would have
--  inconsistent feedback as the returned one. Then it calculates the possible
--  feedback of each next guess and chooses the one that would narrow the possible
--  targets the most. By iterating these process again and again, this program 
--  can guess the result in less than 6 steps and takes no more than 5 seconds.

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess, removeImpossible, 
              calculate) where

-- | Module imported, List for list related functions, Maybe for tranformation
-- between Maybe Value to Value
import Data.List
import Data.Maybe

-- | Type definition of Location and GameState
type Location = (Int, Int)
type GameState = [[Location]]

-- Input: a String that represent location
-- Output: a Location
--
-- | toLocation is to transform a valid string to a location in grid such as A1
-- to (1,1) and return nothing if the string is invalid
toLocation :: String -> Maybe Location
toLocation str = 
    if length str == 2 && isJust index_c && isJust index_r then 
        Just ((fromJust index_c)+1, (fromJust index_r)+1)
    else Nothing
        where 
            columns = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
            rows = ['1', '2', '3', '4']
            index_c = elemIndex (head str) columns
            index_r = elemIndex (last str) rows
            
-- Input: a Location
-- Output: a corresponding String
--
-- | fromLocation is to transform a Location to its corresponding string, such as
-- from (1,1) to A1
fromLocation :: Location -> String
fromLocation (m, n) =  [columns !! (m-1)] ++ [rows !! (n-1)]
    where 
        columns = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
        rows = ['1', '2', '3', '4']
        
-- Input: a Location list of targets and a Location list of guesses
-- Output: a (Int, Int, Int) represent the distance relationships
--
-- | feedback is to return the answer of the given target and guess, which reveal
-- the distances between the location of guess and target
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback targets guesses = (zeros, ones - zeros, twos - ones)
    where 
        zeros = length [x | x <- guesses, elem x targets]
        ones = length [x | x <- guesses, inRange 1 x targets]
        twos = length [x | x <- guesses, inRange 2 x targets]

-- Input: a Int represent range, a target Location and a Location list
-- Output: a Bool which is the judgement of whether the target is in that range 
-- of those Locations
--
-- | inRange is to judge if the distance from a location to a list of locations
-- is within specific range and it is used to calculate the value of feedback
inRange :: Int -> Location -> [Location] -> Bool
inRange _ _ [] = False
inRange n (x,y) ((x1,y1):xys)
    | -n <= x-x1 && x-x1 <= n && -n <= y-y1 && y-y1 <= n = True
    | otherwise = inRange n (x,y) xys

-- Input: No Input here
-- Output: a ([Location], GameState) where [Locaiton] is a 3 elements list contains
-- the initial guess and the GameState is the type that carries needed information
--
-- | initialGuess is to return the basic GameState settings and the first guess
-- A1 A2 A3 are chose here to narrow the range so that it has more possibility to
-- get the feedback like (0,0,0). Then less candidates will be left for future 
-- calculation.
initialGuess :: ([Location], GameState)
initialGuess = ([(1, 1), (1, 2), (1, 3)], beginState)
    where
        initials = [fromLocation (x, y) | x <- [1..8], y <- [1..4]]
        beginState = [map fromJust (map toLocation [x, y, z]) | 
            x <- initials, y <- initials, y > x, z <- initials, z > y]

-- Input: a ([Location], GameState) where [Location] is the previous guess and
-- the GameState is the previous state and a (Int, Int, Int) which represents the
-- feedback of that guess.
-- Output: a ([Location], GameState) where the [Location] is the next guess and
-- the GameState has been filtered to leave much less candidates.
--
-- | nextGuess is to choose the next guess based on the feedback from the previous
-- guess. First it will remove the candidates that would have inconsistent feedback 
-- and calculate the possible amount of candidates for each possible target. Then 
-- the average number will be calculated. At last it will choose the guess that 
-- would cause the number of left candidates to be the smallest, which is the one 
-- that narrow the range the most.
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess (guesses, state) answer = (newGuesses, newState)
    where 
        newState = removeImpossible (guesses, state) answer
        probAnswer = [(x, y, z) | x <- [0..3], y <- [0..3], z <- [0..3], x+y+z <= 3]
        guessesWithProb = sortOn fst [((calculate x newState probAnswer), x) | x <- newState]
        (_, newGuesses) = head guessesWithProb
        
-- Input: a ([Location], GameState) where [Location] is the previous guess and
-- the GameState is the previous state and a (Int, Int, Int) which represents the
-- feedback of that guess.
-- Output: a GameState that has been filtered to leave much less candidates.
--
-- | removeImpossible is to remove all the candidates that would have inconsistent 
-- feedback by using the funtion feedback to test if they are the same and only
-- save the same one as next candidates.
removeImpossible :: ([Location], GameState) -> (Int, Int, Int) -> GameState
removeImpossible (guesses, state) answer = [x | x <- state, feedback x guesses == answer]

-- Input: a [Location] and a GameState which represents possible next guess and 
-- temporary state, also there is a [(Int, Int, Int)] which is the list of possible
-- feedback of that possible guess
-- Output: a Int which represent the average number of candidates left when doing
-- this next guess
--
-- | calculate is to calculate the average number of candidates of a specific guess
-- by using removeImpossible to get the number of candidates of each possible answer
-- and divide the sum of the number by the number of possible answer.(answer that
-- cause no candidates left is impossible)
calculate :: [Location] -> GameState -> [(Int, Int, Int)] -> Int
calculate guesses remains probAnswer = 
    if length remains < 800 then div (sum removeInvalid) (length removeInvalid)
    else 0
        where
            candidateNum = [length (removeImpossible (guesses, remains) x) | x <- probAnswer]
            removeInvalid = [x | x <- candidateNum, not (x == 0)]
