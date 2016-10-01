{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Network.Wreq 
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.ByteString (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.UTF8 (toString)
import System.IO
import System.Exit
import Control.Exception
import System.Environment
import Control.Monad
import Data.Maybe
import System.Process

optionsList = ["Post", "User", "Subject", "Privacy", "Mood"]

optionHTML "User" = "user"
optionHTML "Subject" = "subject"
optionHTML "Privacy" = "security"
optionHTML "Mood" = "prop_current_moodid"
optionHTML "Post" = "event"
 

findOption :: String -> String -> Maybe String
findOption opt body
 | (find (\x -> ("@" ++ opt) `isPrefixOf` x) (lines body)) == Nothing = Just ""
 | otherwise = do 
     x <- find (\x -> ("@" ++ opt) `isPrefixOf` x) (lines body)
     return $ drop ((length opt) + 2) x

getPostText :: String -> String
getPostText body = unlines $ dropWhile (\x -> ("--" `isPrefixOf` x) || ("@" `isPrefixOf` x)) (lines body) 

generateOptions body = sequence $ ((Just (getPostText body)) : (map (\x -> findOption x body) (tail $ optionsList)))

getLocalTime :: IO (String, String, String, String, String) 
getLocalTime = do     
    t <- getZonedTime
    let (year, month, day) = toGregorian $ localDay $ zonedTimeToLocalTime t 
    let hours = todHour $ localTimeOfDay $ zonedTimeToLocalTime t
    let mins = todMin $ localTimeOfDay $ zonedTimeToLocalTime t
    return (show year, show month, show day, show hours, show mins)

getPassword :: IO String
getPassword = do
  putStr "password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getAddress :: [String] -> String
getAddress args
 | elem "-p" args = "http://lj.rossia.org/preview/entry.bml"
 | otherwise = "http://lj.rossia.org/update.bml"

getPostFilename :: [String] -> String
getPostFilename [] = ""
getPostFilename ("-t":xs) = head xs
getPostFilename (x:xs) = getPostFilename xs

main :: IO ()
main = do
  args <- getArgs
  let isPreview = elem "-p" args
  let address = getAddress args
  let postF = getPostFilename args
  if (postF == "") 
   then do
    putStrLn "no post file specified"
    exitFailure
   else return ()
  bodytext <- readFile postF
  let Just opts = generateOptions bodytext 
  let postopts = zipWith (\x y -> ((optionHTML x) := y)) optionsList opts
  password <- getPassword
  (year, month, day, hours, mins) <- getLocalTime
  r <- post address (postopts ++ [ "password" := password
                    , "date_ymd_mm" := month
                    , "date_ymd_yyyy" := year
                    , "date_ymd_dd" := day
                    , "hour" := hours
		    , "min" := mins
		    ])
  let body = toString $ toStrict $ r ^. responseBody
  if isPreview then do 
    previewFile <- openFile "/tmp/preview.html" WriteMode
    hPutStr previewFile ("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n" ++ body)
    hClose previewFile
    exc <- system ("xdg-open /tmp/preview.html")
    exitWith exc
    else if (isInfixOf "you've posted" body)
      then do putStr "posted.\n"
      else do putStr "error.\n"
