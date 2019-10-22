module Main where
import Markov
import Request
import Data.List
import Data.Char
import Data.Time
import System.Directory


displayRawWeatherData weatherData = do
    putStrLn "Raw Weather Data: "
    putStrLn weatherData

writeRawWeatherData weatherData = do
    putStrLn "Enter the filename you want to write to (without extension): "
    filename <- getLine 
    path <- getCurrentDirectory
    putStrLn ("Writing to " ++ path ++ "\\" ++ filename ++ ".txt")
    writeFile (filename ++ ".txt") weatherData
    displayRawWeatherData weatherData

getRawWeatherData = do
    putStrLn "Loading Raw Weather Data....."
    dataW <- getRawWeatherForecast
    putStrLn "Would you like to write data to textfile? (y/n): "
    answer <- getLine 
    case (toLower (head answer)) of
        'y' -> writeRawWeatherData dataW
        'n' -> displayRawWeatherData dataW
        _ -> error "Invalid Input!"
    
main = do
    putStrLn "================================================="
    putStrLn "Welcome to CPSC 312 Weather Predictor"
    putStrLn "What would you like to do?"
    putStrLn "0 - Get raw weather data"
    putStrLn "1 - Get weather forecast summary"
    putStrLn "2 - Get weather prediction for next day"
    putStrLn "3 - Get weekly weather prediction"
    putStrLn "4 - Get infinite prediction [WARNING - Will not stop unless you manually exit]"
    putStrLn "================================================="
    option <- getLine
    weatherSummary <- getWeatherForecastSummaryVerbose False
    case (read option) of 
        0 -> getRawWeatherData
        1 -> putStrLn (show weatherSummary)
        2 -> displayNextPrediction
        3 -> displayWeeklyPrediction
        4 -> infinitePrediction
        _ -> error "Error on parsing input!"
    putStrLn "Done! \n"
    main

    
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
    weatherSummary <- getWeatherForecastSummaryVerbose False
    putStrLn (show weatherSummary)

nextPrediction dataW = do
    next <- (markov dataW)
    return next

infinitePrediction dataW = do
    next <- markov dataW
    infinitePrediction (dataW ++ [next])