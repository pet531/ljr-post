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
--import Data.String.Utils

user = "pet531" :: String
privacy = "private" :: String
mood = "90" :: String

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
  entrytext <- readFile postF 
  password <- getPassword
  (year, month, day, hours, mins) <- getLocalTime 
  r <- post address [ "user" := user 
                                              , "password" := password
                                              , "event" := entrytext
                                              , "security" := privacy
                                              , "date_ymd_mm" := month
                                              , "date_ymd_yyyy" := year
                                              , "date_ymd_dd" := day
                                              , "hour" := hours
                                              , "min" := mins
					      , "prop_current_moodid" := mood
					      ]
  let body = toString $ toStrict $ r ^. responseBody
  if isPreview then do 
    previewFile <- openFile "preview.html" WriteMode
    hPutStr previewFile body
    hClose previewFile
    else if (isInfixOf "you've posted" body)
      then do putStr "posted.\n"
      else do putStr "error.\n"
  putStrLn body
  putStrLn $ show r
