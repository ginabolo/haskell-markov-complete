module Markov where
import System.Random
import Data.List

-- Assume observation is one of "sunny", "cloudy", "rain", "snow" for now
type Observation = [Char]

-- Input: [Observation]
-- Output: Observation: predicted next state

-- State is a cluster of n observations
-- Choose n = 3
data State = StateV {
one :: Observation,
two :: Observation ,
three :: Observation
}
    deriving (Ord, Eq, Show)

-- A table fo transition probabilities is a function that takes a source state and a 
-- target state and returns the probability of that transition
type TransitionTable f = (State -> State -> f)






-- Build markov chain, then predict the next state
markov :: [Observation] -> IO ()
markov lst = do
    rand <- (randomIO :: IO Double) 
    putStrLn (show (predictSample rand (computeTT states) (last states) states))
    where 
        states = observationsToStates(lst)




observationsToStates :: [Observation] -> [State]
observationsToStates [] = []
observationsToStates (f:s:t:tail) = StateV f s t : (observationsToStates (s:t:tail))
-- if there aren't enough to make a full new state
observationsToStates lst = []






emptyTT :: (Fractional f) => TransitionTable f
emptyTT _ _ = 0

-- Outputs a TransitionTable of transition from state to state
computeTT :: (Fractional f) => [State] -> TransitionTable f
computeTT [] fromState toState = emptyTT fromState toState
computeTT datal fromState toState = computeTThelper datal 0 0 fromState toState

computeTThelper :: (Fractional f) =>  [State] -> f -> f -> TransitionTable f
computeTThelper [] numDatapoints numTransitions fromState toState = numTransitions/numDatapoints
computeTThelper (h:s:t) numDatapoints numTransitions fromState toState
    -- this is an instance of the transition we are looking for
    | fromState == h && toState == s = computeTThelper (s:t) (numDatapoints+1) (numTransitions+1) fromState toState
    -- this is an instance of a transitition from the state of interest to another state
    | fromState == h  = computeTThelper (s:t) (numDatapoints+1) numTransitions fromState toState
    -- irrelevant sample
    | otherwise       = computeTThelper (s:t) numDatapoints numTransitions fromState toState

computeTThelper lst numDatapoints numTransitions fromState toState = numTransitions/numDatapoints








-- Unroll markov chain: TransitionTable -> last State in sequence of Observations -> possible states -> predicted Observation
-- Select the most likely outcome
predictMax :: (Ord f, Fractional f) => TransitionTable f -> State -> [State] -> Observation
predictMax table s possibleStates = three (state (foldl (\ acc outcome -> max acc outcome) (OutcomeV 0 (head possibleStates)) (probOfStates (table s) possibleStates)))


predictSample :: (Ord f, Fractional f) => f -> TransitionTable f -> State -> [State] -> Observation
-- Sample from outcome probability distribution
predictSample rand table s possibleStates = 
    if (length filteredDistribution) > 0
        then three (state (sample rand (sort filteredDistribution)))
        else three (state (head distribution)) 
        -- if the last state has never been seen, pick a random one
    where 
        distribution = (probOfStates (table s) possibleStates)
        filteredDistribution = filter (\ out -> prob out > 0) distribution

sample :: (Ord f, Fractional f) => f -> [Outcome f] -> Outcome f
-- rand in a random number [0, 1]
sample rand lst 
    | length lst == 1       = (head lst)
    | rand < prob (head lst) = (head lst)
    | otherwise     = sample (rand - (prob (head lst))) (tail lst)


data Outcome f = OutcomeV {
    prob :: f,
    state :: State
} deriving (Ord, Eq, Show)


probOfStates :: (Ord f, Fractional f) => (State -> f) -> [State] -> [Outcome f]
-- return tuples of the probability of each state given with the state
probOfStates _ [] = [] 
probOfStates table (h:t) = (OutcomeV (table h) h):(probOfStates (table) t) 




-- -----------------------------------------------------
-- Util:
test :: (Show a, Eq a) => [Char] -> a -> a -> [Char]
test msg a b 
    | a == b    = msg ++ " pass"
    | otherwise = msg ++ show a ++ " does not match " ++ show b


-- Tests:
run_tests = do
    putStrLn (test "observationsToStates" (observationsToStates ["sunny", "cloudy", "rain", "snow", "sunny", "cloudy", "rain", "snow" ]) 
        [(StateV "sunny" "cloudy" "rain"), (StateV "cloudy" "rain" "snow"), (StateV "rain" "snow" "sunny"), (StateV "snow" "sunny" "cloudy"), 
        (StateV "sunny" "cloudy" "rain"), (StateV "cloudy" "rain" "snow") ])

    putStrLn (test "compute transition table sunny" ((computeTT [(StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "cloudy") ])
        (StateV "sunny" "sunny" "sunny") (StateV "sunny" "sunny" "sunny")) (2/3))

    putStrLn (test "compute transition table cloudy" ((computeTT [(StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "cloudy") ])
        (StateV "sunny" "sunny" "sunny") (StateV "sunny" "sunny" "cloudy")) (1/3))

    putStrLn (test "probOfStates" (probOfStates (computeTT [(StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "cloudy") ] (StateV "sunny" "sunny" "sunny"))
        [(StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "cloudy")]) 
        [(OutcomeV (2/3) (StateV "sunny" "sunny" "sunny")), (OutcomeV (1/3) (StateV "sunny" "sunny" "cloudy"))])

    putStrLn (test "predict" (predictMax (computeTT [(StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "cloudy") ])
        (StateV "sunny" "sunny" "sunny") [(StateV "sunny" "sunny" "sunny"), (StateV "sunny" "sunny" "cloudy")])
        "sunny")

    -- markov ["sunny", "cloudy", "cloudy", "cloudy", "sunny", "sunny", "sunny", "cloudy", "cloudy", "sunny", "cloudy", "cloudy"]
    -- 50/50 chance of being sunny or cloudy 
