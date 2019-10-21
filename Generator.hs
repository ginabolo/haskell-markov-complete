module Generator where
import System.Random

generateWeather :: (Num a, Enum a) => a -> [String] -> [IO String]
generateWeather length possibilities = do
    generated <- map (\e -> pickRandom possibilities) [1..length]
    return generated

pickRandom :: [String] -> IO String
pickRandom possibilities = do
    randomIndex <- randomRIO (0, length possibilities - 1)
    return (possibilities !! randomIndex)
