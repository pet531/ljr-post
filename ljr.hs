{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Network.Wreq 
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import System.IO
import Control.Exception

user = "pet531" :: String
entrytext = "test" ++ "\n" ++ "test" ++ "\n" ++ "test" :: String
privacy = "private" :: String

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

main :: IO ()
main = do
  password <- getPassword
  (year, month, day, hours, mins) <- getLocalTime  
  r <- post "http://lj.rossia.org/update.bml" [ "user" := user 
                                              , "password" := password
                                              , "event" := entrytext
                                              , "security" := privacy
                                              , "date_ymd_mm" := month
                                              , "date_ymd_yyyy" := year
                                              , "date_ymd_dd" := day
                                              , "hour" := hours
                                              , "min" := mins
					      ]
  if (isInfixOf "you've posted" (show r))
    then putStr "posted.\n"
    else putStr "error.\n"
  --putStrLn $ show r
