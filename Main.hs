module Main where
import Markov
import Request

--displayRawWeatherData :: IO()
displayRawWeatherData = do
    putStrLn "Raw Weather Data: "
    dataW <- getRawWeatherForecast
    putStrLn dataW

--displayInfinitePrediction :: IO()
displayInfinitePrediction = do
    putStrLn "Welcome to the CPSC 312 Weather Predictor!"
    weatherSummary <- getWeatherForecastSummary
    putStrLn "In the last 50 hours, the weather has been: "
    putStrLn (show weatherSummary)
    putStrLn "Infinite markov chain prediction with past 50 hours of data: "
    putStrLn (nextPrediction weatherSummary)

nextPrediction dataW

inifinitePrediction dataW = do
    next <- markov dataW
    inifinitePrediction (dataW ++ [next])

main = do putStrLn ""