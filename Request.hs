module Request where
import Network.HTTP
import Network.Stream
import Control.Concurrent

getRawWeatherForecast :: IO String
getRawWeatherForecast = do
    response <- simpleHTTP (getRequest "http://www.johnturkson.com/weather/get")
    case (fmap rspBody response) of
        Left err -> return ""
        Right result -> return result

getWeatherForecast :: IO [String]
getWeatherForecast = do
    forecast <- getRawWeatherForecast
    return (parseWeatherForecast forecast)

getWeatherForecastSummary :: IO [String]
getWeatherForecastSummary = do
    putStrLn "Getting weather data from API..."
    forecast <- getRawWeatherForecast
    --saveWeatherForecast (getLines forecast) "raw.json"
    putStrLn (show forecast)
    putStrLn "Parsing weather data..."
    putStrLn (show (parseWeatherForecast forecast))
    let parsed = parseWeatherForecast forecast
    --saveWeatherForecast parsed "forecast.json"
    putStrLn "Constructing forecast..."
    putStrLn (show (map snd (getAllMatching "summary" (map extractKeyValue parsed))))
    let summary = map snd (getAllMatching "summary" (map extractKeyValue parsed))
    --saveWeatherForecast summary "summary.json"
    return summary

parseWeatherForecast :: String -> [String]
parseWeatherForecast forecast = (removeEmpty (removeBrackets (map trimAll (getLines (extractSection "hourly" forecast)))))

findSection :: String -> String -> String -> Char -> String
findSection sectionName text delimiter openingBracketType = findSectionHelper ((surroundInQuotes sectionName) ++ delimiter ++ [openingBracketType]) text ""

findSectionHelper :: String -> String -> String -> String
findSectionHelper sectionName text search
    | text == "" = ""
    | (length search) == (length sectionName) = if (search == sectionName) then text else (findSectionHelper sectionName (tail text) ((tail search) ++ [(head text)]))
    | otherwise = findSectionHelper sectionName (tail text) (search ++ [(head text)])

extractSection :: String -> String -> String
extractSection sectionName text = extractSectionParameterized sectionName text ": " '{' '}'

extractSectionParameterized :: String -> String -> String -> Char -> Char -> String
extractSectionParameterized sectionName text delimiter openingBracketType closingBracketType = extractSectionHelper (findSection sectionName text delimiter openingBracketType) openingBracketType closingBracketType 1 ""

extractSectionHelper :: (Eq a, Num a) => String -> Char -> Char -> a -> String -> String
extractSectionHelper text openingBracketType closingBracketType bracketCount extracted
    | bracketCount == 0 = extracted
    | (length text == 0) = ""
    | (head text) == openingBracketType = extractSectionHelper (tail text) openingBracketType closingBracketType (bracketCount + 1) (extracted ++ [(head text)])
    | (head text == closingBracketType) = extractSectionHelper (tail text) openingBracketType closingBracketType (bracketCount - 1) (extracted ++ [(head text)])
    | otherwise = extractSectionHelper (tail text) openingBracketType closingBracketType bracketCount (extracted ++ [(head text)])

saveWeatherForecast :: [String] -> FilePath -> IO ()
saveWeatherForecast forecast file = do
    writeFile file (foldr (++) "" (map (\line -> line ++ ['\n']) forecast))

loadWeatherForecast :: FilePath -> IO [String]
loadWeatherForecast file = do
    forecast <- readFile file
    return (getLines forecast)

getBlock :: String -> String
getBlock text = extractSectionParameterized "" text "" '{' '}'

getLines :: String -> [String]
getLines text = getLinesHelper text ""

getLinesHelper :: String -> String -> [String]
getLinesHelper "" line = [line]
getLinesHelper ('\r':'\n':remaining) line = line : (getLinesHelper remaining "")
getLinesHelper ('\n':remaining) line = line : (getLinesHelper remaining "")
getLinesHelper (start:remaining) line = getLinesHelper remaining (line ++ [start])

trimFrontWhitespace :: String -> String
trimFrontWhitespace "" = ""
trimFrontWhitespace (' ':remaining) = trimFrontWhitespace remaining
trimFrontWhitespace ('\t':remaining) = trimFrontWhitespace remaining
trimFrontWhitespace remaining = remaining

trimIfFront :: (Char -> Bool) -> String -> String
trimIfFront predicate "" = ""
trimIfFront predicate (start:remaining) = if (predicate start) then (trimIfFront predicate remaining) else (start:remaining)

trimIf :: (Char -> Bool) -> String -> String
trimIf predicate text = (trimIfEnd predicate (trimIfFront predicate text))

trimIfEnd :: (Char -> Bool) -> String -> String
trimIfEnd predicate text = trimIfEndHelper predicate text "" ""

trimIfEndHelper :: (Char -> Bool) -> String -> String -> String -> String
trimIfEndHelper predicate text portion trimmed
    | (text == "") = trimmed
    | (predicate (head text)) = trimIfEndHelper predicate (tail text) (portion ++ [(head text)]) trimmed
    | otherwise = trimIfEndHelper predicate (tail text) "" (trimmed ++ portion ++ [(head text)])

trimEndWhitespace :: String -> String
trimEndWhitespace text = trimEndHelper text "" ""

trimEndHelper :: String -> String -> String -> String
trimEndHelper text portion trimmed
    | (text == "") = trimmed
    | (head text == ' ') = trimEndHelper (tail text) (portion ++ " ") trimmed
    | (head text == '\t') = trimEndHelper (tail text) (portion ++ ['\t']) trimmed
    | otherwise = trimEndHelper (tail text) "" (trimmed ++ portion ++ [(head text)])

trimWhitespace :: String -> String
trimWhitespace = trimFrontWhitespace . trimEndWhitespace

trimBrackets :: String -> String
trimBrackets = trimBracketsFront . trimBracketsEnd

trimBracketsFront :: String -> String
trimBracketsFront "" = ""
trimBracketsFront ('{':remaining) = trimBracketsFront remaining
trimBracketsFront ('}':remaining) = trimBracketsFront remaining
trimBracketsFront ('[':remaining) = trimBracketsFront remaining
trimBracketsFront (']':remaining) = trimBracketsFront remaining
trimBracketsFront ('(':remaining) = trimBracketsFront remaining
trimBracketsFront (')':remaining) = trimBracketsFront remaining

trimBracketsEnd :: String -> String
trimBracketsEnd = trimIfEnd (\c -> c `elem` "{}[]()")

trimCommas :: String -> String
trimCommas text = (trimCommasEnd (trimCommasFront text))

trimCommasFront :: String -> String
trimCommasFront = trimIfFront (\c -> c == ',')

trimCommasEnd :: String -> String
trimCommasEnd = trimIfEnd (\c -> c == ',')

trimQuotes :: String -> String
trimQuotes = trimQuotesFront . trimQuotesEnd

trimQuotesFront :: String -> String
trimQuotesFront = trimIfFront (\c -> c == '\"')

trimQuotesEnd :: String -> String
trimQuotesEnd = trimIfEnd (\c -> c == '\"')

trimAll :: String -> String
trimAll text = (trimAllEnd (trimAllFront text))

trimAllFront :: String -> String
trimAllFront text = trimIfFront (\e -> e `elem` '\t':" {}[](),\"") text

trimAllEnd :: String -> String
trimAllEnd text = trimIfEnd (\e -> e `elem` '\t':" {}[](),\"") text

surroundInQuotes :: String -> String
surroundInQuotes text = "\"" ++ text ++ "\""

removeQuotes :: String -> String
removeQuotes = trimIf (\c -> c == '\"')

removeEmpty :: [String] -> [String]
removeEmpty = filter (\e -> (length e) > 0)

removeBrackets :: [String] -> [String]
removeBrackets = filter (\e -> e /= "{" && e /= "}" && e /= "[" && e /= "]" && e /= "(" && e /= ")")

extractKeyValue :: String -> (String, String)
extractKeyValue pairString = ((extractKey pairString), (extractValue pairString))

extractKey :: String -> String
extractKey pairString = trimAll (foldr (\c acc -> if (c == ':') then "" else c:acc) "" pairString)

extractValue :: String -> String
extractValue pairString = if (trimmed == "") then "" else (trimAll (tail trimmed)) where trimmed = (trimIfFront (\c -> c /= ':') pairString)

getAllMatching :: Eq a => a -> [(a, t)] -> [(a, t)]
getAllMatching key = filter (\(k, v) -> k == key)
