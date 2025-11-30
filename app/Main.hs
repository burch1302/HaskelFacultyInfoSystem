{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Text (Text, unpack, pack)
import Data.Int (Int32)
import Control.Monad (forM_, when)
import System.IO (hFlush, stdout, stdin, hSetEncoding, utf8)

class FromRow a where
    fromRow :: [MySQLValue] -> Maybe a

data Software = Software {
    swId :: Int32,
    swName :: Text,
    swVersion :: Maybe Text,
    swType :: Text,
    swAuthor :: Text,
    swLocation :: Text
} deriving (Show)

data UsageStat = UsageStat {
    usUser :: Text,
    usRole :: Text,
    usSoftName :: Text,
    usTime :: MySQLValue
} deriving (Show)

instance FromRow Software where
    fromRow [MySQLInt32 i, MySQLText n, v, MySQLText t, MySQLText a, MySQLText l] = 
        Just $ Software {
            swId = i,
            swName = n,
            swVersion = case v of { MySQLText txt -> Just txt; _ -> Nothing },
            swType = t,
            swAuthor = a,
            swLocation = l
        }
    fromRow _ = Nothing

instance FromRow UsageStat where
    fromRow [MySQLText u, MySQLText r, MySQLText s, t] = 
        Just $ UsageStat { usUser = u, usRole = r, usSoftName = s, usTime = t }
    fromRow _ = Nothing

getConnInfo :: ConnectInfo
getConnInfo = defaultConnectInfo {
    ciUser = "root",
    ciPassword = "root",
    ciDatabase = "faculty_network",
    ciHost = "127.0.0.1"
}

fetchData :: (FromRow a) => MySQLConn -> Query -> IO [a]
fetchData conn q = do
    (_, stream) <- query_ conn q
    rawRows <- Streams.toList stream
    return [ x | Just x <- map fromRow rawRows ]

showAllSoftware :: MySQLConn -> IO ()
showAllSoftware conn = do
    let q = "SELECT s.id, s.name, s.version, t.type_name, a.name, l.path_or_room \
            \FROM software s \
            \JOIN software_types t ON s.type_id = t.id \
            \JOIN authors a ON s.author_id = a.id \
            \JOIN locations l ON s.location_id = l.id"
    
    rows <- fetchData conn q :: IO [Software]
    
    putStrLn "\n--- SOFTWARE LIST ---"
    putStrLn "------------------------------------------------------------"
    forM_ rows $ \s -> do
        let ver = maybe "" unpack (swVersion s)
        putStrLn $ "[" ++ show (swId s) ++ "] " ++ unpack (swName s) ++ " " ++ ver
                 ++ " | Type: " ++ unpack (swType s)
                 ++ " | Author: " ++ unpack (swAuthor s)
    putStrLn "------------------------------------------------------------"

showStats :: MySQLConn -> IO ()
showStats conn = do
    let q = "SELECT u.username, u.role, s.name, us.access_time \
            \FROM usage_stats us \
            \JOIN users u ON us.user_id = u.id \
            \JOIN software s ON us.software_id = s.id"

    rows <- fetchData conn q :: IO [UsageStat]

    putStrLn "\n--- USAGE STATISTICS ---"
    forM_ rows $ \s -> 
        putStrLn $ "User " ++ unpack (usUser s) ++ " (" ++ unpack (usRole s) ++ ")"
                 ++ " accessed " ++ unpack (usSoftName s)

addSoftware :: MySQLConn -> IO ()
addSoftware conn = do
    putStrLn "\n--- ADD NEW SOFTWARE ---"
    
    putStr "Enter Name: "
    hFlush stdout
    name <- getLine
    
    putStr "Enter Version: "
    hFlush stdout
    ver <- getLine

    putStr "Author ID (1-Microsoft, 2-JetBrains, 3-Adobe, 4-MathWorks): "
    hFlush stdout
    authId <- getLine

    putStr "Type ID (1-OS, 2-IDE, 3-Scientific, 4-Graphics): "
    hFlush stdout
    typeId <- getLine

    let q = "INSERT INTO software (name, version, author_id, type_id, location_id) VALUES (?, ?, ?, ?, 1)"
    
    stmt <- prepareStmt conn q
    _ <- executeStmt conn stmt [MySQLText (pack name), MySQLText (pack ver), MySQLInt32 (read authId), MySQLInt32 (read typeId)]
    putStrLn "Successfully added!"

updateSoftware :: MySQLConn -> IO ()
updateSoftware conn = do
    putStrLn "\n--- EDIT SOFTWARE ---"
    showAllSoftware conn
    
    putStr "Enter ID to edit: "
    hFlush stdout
    sId <- getLine

    putStr "Enter NEW Name: "
    hFlush stdout
    name <- getLine

    putStr "Enter NEW Version: "
    hFlush stdout
    ver <- getLine

    let q = "UPDATE software SET name = ?, version = ? WHERE id = ?"
    
    stmt <- prepareStmt conn q
    _ <- executeStmt conn stmt [MySQLText (pack name), MySQLText (pack ver), MySQLInt32 (read sId)]
    putStrLn "Successfully updated!"

deleteSoftware :: MySQLConn -> IO ()
deleteSoftware conn = do
    putStrLn "\n--- DELETE SOFTWARE ---"
    showAllSoftware conn
    
    putStr "Enter ID to delete: "
    hFlush stdout
    sId <- getLine

    let q = "DELETE FROM software WHERE id = ?"
    
    stmt <- prepareStmt conn q
    _ <- executeStmt conn stmt [MySQLInt32 (read sId)]
    putStrLn "Successfully deleted!"

menuLoop :: MySQLConn -> IO ()
menuLoop conn = do
    putStrLn "\n============================"
    putStrLn "1. Show all software"
    putStrLn "2. Show usage statistics"
    putStrLn "3. Add new software"
    putStrLn "4. Edit software"
    putStrLn "5. Delete software"
    putStrLn "0. Exit"
    putStrLn "============================"
    putStr "Select action: "
    hFlush stdout
    
    choice <- getLine
    case choice of
        "1" -> showAllSoftware conn >> menuLoop conn
        "2" -> showStats conn >> menuLoop conn
        "3" -> addSoftware conn >> menuLoop conn
        "4" -> updateSoftware conn >> menuLoop conn
        "5" -> deleteSoftware conn >> menuLoop conn
        "0" -> putStrLn "Goodbye!"
        _   -> putStrLn "Invalid input, try again." >> menuLoop conn

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8

    putStrLn "Connecting to database..."
    conn <- connect getConnInfo
    putStrLn "Connected! Faculty Information System started."
    
    menuLoop conn
    close conn
