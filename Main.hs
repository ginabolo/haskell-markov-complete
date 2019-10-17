module Main where
import Markov
import Request

displayRawWeatherData :: IO()
displayRawWeatherData = do
    putStrLn "Raw Weather Data: "
    putStrLn getRawWeatherData

displayInfinitePrediction :: IO()
displayInfinitePrediction = do
    putStrLn "Infinite markov chain prediction with past 50 hours of data: "
    putStrLn (nextPrediction getWeatherForecastSummary)

displayTimedPrediction :: IO()
displayTimedPrediction = do
    putStrLn "Enter the number of iterations in the markov chain: "
    iterations <- getLine
    putStrLn "Weather prediction for the next " ++ (show (* (read iterations) 50)) ++ " hours: "
    putStrLn (nextPrediction getWeatherForecastSummary (read iterations))

displayAllDaysWithAttribute :: IO()
displayAllDaysWithAttribute = do
    putStrLn "Enter a weather attribute: "
    attribute <- getLine
    putStrLn "All data for <" ++ attribute ++ ">: " 
    putStrLn (GetAllMatching attribute)  

nextPrediction data = markov data ++ (nextPrediction (markov data))
predictWeather data iteration | iteration > 0 = markov data ++ (predictWeather (markov data) (iteration - 1))
                              | otherwise = data