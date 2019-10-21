module Main where
import Markov
import Request

displayRawWeatherData :: IO()
displayRawWeatherData = do
    putStrLn "Raw Weather Data: "
    dataW <- getRawWeatherData
    putStrLn dataW

displayInfinitePrediction :: IO()
displayInfinitePrediction = do
    putStrLn "Welcome to the CPSC 312 Weather Predictor!"
    weatherSummary <- getWeatherForecastSummary
    putStrLn "In the last 50 hours, the weather has been: " ++ show (weatherSummary)
    putStrLn "Infinite markov chain prediction with past 50 hours of data: "
    putStrLn (nextPrediction getWeatherForecastSummary)

displayPrediction :: IO()
displayPrediction = do
    putStrLn "Welcome to the CPSC 312 Weather Predictor!"
    weatherSummary <- getWeatherForecastSummary
    putStrLn "In the last 50 hours, the weather has been: " ++ show (weatherSummary)
    putStrLn "Please enter the # of iterations in markov chain predictor: "
    iterations <- getLine
    putStrLn "Weather prediction for the next " ++ (show (* (read iterations) 50)) ++ " hours: "
    putStrLn (nextPrediction getWeatherForecastSummary (read iterations))

displayAllDaysWithAttribute :: IO()
displayAllDaysWithAttribute = do
    putStrLn "Enter a weather attribute: "
    attribute <- getLine
    dataW <- getRawWeatherData
    putStrLn "All data for <" ++ attribute ++ ">: " 
    putStrLn (getAllMatching (extractKeyValue dataW))

nextPrediction dataW = markov dataW ++ (nextPrediction (markov dataW))
predictWeather dataW iteration | iteration > 0 = markov dataW ++ (predictWeather (markov dataW) (iteration - 1))
                               | otherwise = dataW
