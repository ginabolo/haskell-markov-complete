module Main where
import Markov
import Request
import Data.List
import Data.Char

--displayRawWeatherData :: IO()
displayRawWeatherData = do
    putStrLn "Raw Weather Data: "
    dataW <- getRawWeatherForecast
    putStrLn dataW

--displayInfinitePrediction :: IO()
displayNextPrediction = do
    putStrLn "Welcome to CPSC 312 Weather Predictor!"
    weatherSummary <- getWeatherForecastSummaryVerbose False
    putStrLn "Today's weather has been: "
    putStrLn (show weatherSummary)
    putStrLn "Tommorow's weather will be: "
    next <- nextPrediction weatherSummary
    putStrLn next
    if ((queryWeather weatherSummary "rain") > (queryWeather weatherSummary "sunny")) then putStrLn "Its going to be rainy out. Make sure to bring an umbrella!" else putStrLn ""
    if ((queryWeather weatherSummary "cloudy") > (queryWeather weatherSummary "sunny")) then putStrLn "Its going to be cloudy out, no need for sunscreen!" else putStrLn "Its going to be sunny out, bring some sunscreen!"

displayWeeklyPrediction = do
    putStrLn ""

queryWeather weatherSummary attr = (length (filter (\e -> isInfixOf attr e) weatherSummary))

nextPrediction dataW = do
    next <- (markov dataW)
    return next

infinitePrediction dataW = do
    next <- markov dataW
    infinitePrediction (dataW ++ [next])

main = do putStrLn ""