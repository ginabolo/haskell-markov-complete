module Main where
import Markov
import Request
import Data.List
import Data.Char
import Data.Time

displayRawWeatherData = do
    putStrLn "Raw Weather Data: "
    dataW <- getRawWeatherForecast
    putStrLn dataW

main = do
    weatherSummary <- getWeatherForecastSummaryVerbose False
    putStrLn "Welcome to CPSC 312 Weather Predictor"
    putStrLn "What would you like to do?"
    putStrLn "0 - Get raw weather data"
    putStrLn "1 - Get weather forecast summary"
    putStrLn "2 - Get weather prediction for next day"
    putStrLn "3 - Get weekly weather prediction"
    option <- getLine
    if (option = '0') then displayRawWeatherData else putStrLn ""
    if (option = '1') then (show weatherSummary) else putStrLn ""
    if (option = '2') then displayNextPrediction else putStrLn ""
    if (option = '3') then displayWeeklyPrediction else putStrLn ""

    
displayNextPrediction = do
    weatherSummary <- getWeatherForecastSummaryVerbose False
    putStrLn "The weekly weather forecast has been: "
    putStrLn (show weatherSummary)
    putStrLn "Tommorow's weather will be: "
    next <- nextPrediction weatherSummary
    putStrLn next
    if (isInfixOf "rain" next) then putStrLn "Its going to be rainy out. Make sure to bring an umbrella!" else putStrLn ""
    if (isInfixOf "cloudy" next) then putStrLn "Its going to be cloudy out, no need for sunscreen!" else putStrLn ""
    if (isInfixOf "sunny" next) then putStrLn "Its going to be sunny out, bring some sunscreen!" else putStrLn ""

displayWeeklyPrediction = do
    putStrLn "The weekly weather forecast has been: "
    putStrLn (show weatherSummary)

nextPrediction dataW = do
    next <- (markov dataW)
    return next

infinitePrediction dataW = do
    next <- markov dataW
    infinitePrediction (dataW ++ [next])

main = do putStrLn ""