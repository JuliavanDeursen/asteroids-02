-- | This module handles loading and saving of high scores 
module HighScores where

import System.IO (readFile')
import Control.Exception (catch, IOException)
import Data.List (sortBy)
import Text.Read (readMaybe)

highScoreFile :: FilePath
highScoreFile = "highscores.txt"

-- | loads the top 10 existing high scores
loadHighScores :: IO [Int]
loadHighScores = do
    contents <- catch
        (readFile' highScoreFile)
        handleError

    let scores = mapMaybeRead (lines contents)

    return (take 10 (sortBy (flip compare) scores)) 

-- | converts list of strings into ints
mapMaybeRead :: [String] -> [Int]
mapMaybeRead [] = []
mapMaybeRead (x:xs) =
    case readMaybe x of
        Just n  -> n : mapMaybeRead xs
        Nothing -> mapMaybeRead xs

handleError :: IOException -> IO String
handleError _ = return ""

saveHighScores :: [Int] -> IO ()
saveHighScores scores =
    writeFile highScoreFile (unlines (map show scores))

addHighScore :: Int -> [Int] -> [Int]
addHighScore newScore scores =
    take 10 (sortBy (flip compare) (newScore : scores))