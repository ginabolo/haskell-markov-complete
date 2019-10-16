module Request where
import Network.HTTP
import Network.Stream

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

parseWeatherForecast :: String -> [String]
parseWeatherForecast forecast = removeEmpty (removeBrackets (map trimWhitespace (getLines (extractSection "daily" forecast))))

findSection :: String -> String -> String -> Char -> String
findSection sectionName text delimiter openingBracketType = findSectionHelper ((surroundInQuotes sectionName) ++ delimiter ++ [openingBracketType]) text ""

findSectionHelper sectionName text search
    | text == "" = ""
    | (length search) == (length sectionName) = if (search == sectionName) then text else (findSectionHelper sectionName (tail text) ((tail search) ++ [(head text)]))
    | otherwise = findSectionHelper sectionName (tail text) (search ++ [(head text)])

extractSection :: String -> String -> String
extractSection sectionName text = extractSectionParameterized sectionName text ": " '{' '}'

extractSectionParameterized :: String -> String -> String -> Char -> Char -> String
extractSectionParameterized sectionName text delimiter openingBracketType closingBracketType = extractSectionHelper (findSection sectionName text delimiter openingBracketType) openingBracketType closingBracketType 1 ""

extractSectionHelper text openingBracketType closingBracketType bracketCount extracted
    | bracketCount == 0 = extracted
    | (length text == 0) = ""
    | (head text) == openingBracketType = extractSectionHelper (tail text) openingBracketType closingBracketType (bracketCount + 1) (extracted ++ [(head text)])
    | (head text == closingBracketType) = extractSectionHelper (tail text) openingBracketType closingBracketType (bracketCount - 1) (extracted ++ [(head text)])
    | otherwise = extractSectionHelper (tail text) openingBracketType closingBracketType bracketCount (extracted ++ [(head text)])

saveWeatherForecast :: [String] -> FilePath -> IO ()
saveWeatherForecast forecast file = do
    writeFile file (foldr (++) "" (map (\line -> line ++ ['\n']) forecast))

loadWeatherForecast :: FilePath -> IO String
loadWeatherForecast file = do
    forecast <- readFile file
    return forecast

-- getBlocks

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

--trimFrontIf :: String -> (Char -> Bool) -> String
trimFrontIf predicate "" = ""
trimFrontIf predicate (start:remaining) = if (predicate start) then (trimFrontIf predicate remaining) else (start:remaining)

--trimEndIf :: String -> String
trimEndIf predicate text = trimEndIfHelper predicate text "" ""

trimIf predicate text = (trimEndIf predicate (trimFrontIf predicate text))

-- trimEndHelper ::
trimEndIfHelper predicate text portion trimmed
    | (text == "") = trimmed
    | (predicate (head text)) = trimEndIfHelper predicate (tail text) (portion ++ [(head text)]) trimmed
    | otherwise = trimEndIfHelper predicate (tail text) "" (trimmed ++ portion ++ [(head text)])

trimEndWhitespace :: String -> String
trimEndWhitespace text = trimEndHelper text "" ""

-- trimEndHelper ::
trimEndHelper text portion trimmed
    | (text == "") = trimmed
    | (head text == ' ') = trimEndHelper (tail text) (portion ++ " ") trimmed
    | (head text == '\t') = trimEndHelper (tail text) (portion ++ ['\t']) trimmed
    | otherwise = trimEndHelper (tail text) "" (trimmed ++ portion ++ [(head text)])

trimWhitespace :: String -> String
trimWhitespace = trimFrontWhitespace . trimEndWhitespace

--trimBrackets = trimBracketsFront . trimBracketsEnd

trimBracketsFront :: String -> String
trimBracketsFront "" = ""
trimBracketsFront ('{':remaining) = trimBracketsFront remaining
trimBracketsFront ('}':remaining) = trimBracketsFront remaining
trimBracketsFront ('[':remaining) = trimBracketsFront remaining
trimBracketsFront (']':remaining) = trimBracketsFront remaining
trimBracketsFront ('(':remaining) = trimBracketsFront remaining
trimBracketsFront (')':remaining) = trimBracketsFront remaining

--trimBracketsEnd :: String -> String
--trimBracketsEnd = 

trimCommas :: String -> String
trimCommas "" = ""
--trimCommas 

--trimAll :: String -> String
--trimAll text = trimIf text (\e -> e `elem` '\t':" {}[](),")

surroundInQuotes :: String -> String
surroundInQuotes text = "\"" ++ text ++ "\""

removeEmpty :: [String] -> [String]
removeEmpty = filter (\e -> (length e) > 0)

--removeBrackets :: [String] -> [String]
removeBrackets = filter (\e -> e /= "{" && e /= "}" && e /= "[" && e /= "]" && e /= "(" && e /= ")")

--partitionKeyValue pairString = foldl (\(k, v) c -> if ((length v) > 0) then (k, v ++ [c]) else (if (c == ':') then (k, ":") else (k ++ [c], v))) "" pairString

extractKey pairString = foldr (\c acc -> if (c == ':') then "" else c:acc) "" pairString

extractValue pairString = if (trimmed == "") then "" else (tail trimmed) where trimmed = (trimFrontIf (\c -> c /= ':') pairString)



--getKeys keys mapping

--getKey key mapping
