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
    weatherSummary <- getWeatherForecastSummaryVerbose False
    putStrLn "In the last 50 hours, the weather has been: "
    putStrLn (show weatherSummary)
    putStrLn "Infinite markov chain prediction with past 50 hours of data: "
    next <- nextPrediction weatherSummary
    putStrLn next

nextPrediction dataW = do
    next <- (markov dataW)
    return next

inifinitePrediction dataW = do
    next <- markov dataW
    inifinitePrediction (dataW ++ [next])

main = do putStrLn ""