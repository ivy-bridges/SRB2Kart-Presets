

module Main where

import System.Environment
import System.Directory
import System.IO
import Control.Monad


main :: IO ()
main = do
    modlist:args <- getArgs -- get name of cfg file
    
    paths <- readsettings -- get file paths
    
    let kartFolder = head paths
    putStrLn kartFolder
    let dlFolder = kartFolder ++ "/Download"
    
    
    
    createDirectoryIfMissing False (dlFolder++"/"++modlist) -- create destination folder
    createDirectoryIfMissing False (dlFolder++"/"++modlist++"/CFGs") -- and cfg destination folder
    
    let destination = dlFolder ++ "/" ++ modlist
    
    putStrLn "Getting list of mods..."
    setCurrentDirectory kartFolder
    
    
    copyFile modlist (destination++"/CFGs/"++modlist) -- copy cfg file to dest
    files <- modfiles modlist -- get list of mods
    
   
    setCurrentDirectory dlFolder -- move to download folder
    mapM_ (copymod kartFolder destination) files
    
    putStrLn ("Copied contents of " ++ modlist ++ " to " ++ destination)


-- grabs the destination from settings.txt
readsettings :: IO [String]
readsettings = do
    contents <- readFile "settings.txt"
    
    
    let paths = lines contents
    
    print paths
    return paths

-- grab cfgs and addons from a cfg file
modfiles :: FilePath -> IO [String]
modfiles filename = do
    contents <- readFile filename
    let commands = lines contents  
        execs = filter (\i -> take 4 i == "exec") commands -- only lines starting with exec. these denote additional cfg files
        cfgs = map (drop 5) execs -- just get the filename
        
        addfiles = filter (\i -> take 7 i == "addfile") commands -- only lines starting with "addfile"
        files = cfgs ++ (map (drop 8) addfiles) -- just get the filename
    
    -- recurses into referenced cfg files and adds them to mods list
    additionals <- mapM modfiles cfgs
    
    return (foldl (++) files additionals) -- combine the additionals with the base list and wrap them for IO
    
    


copymod :: String -> String -> String -> IO ()
copymod home dir mod = do
    let copyDest = if isCFG mod 
                   then dir++"/CFGs/"++mod
                   else dir++"/"++mod
    
    alreadyThere <- doesFileExist copyDest
    --putStrLn mod
    --print (storedInHome mod)
    
  
    unless alreadyThere (do 
        if storedInHome mod
            then withCurrentDirectory home (copyFile mod copyDest)
        else copyFile mod copyDest)

-- cfg, soc, and the bonuschars.kart files are all stored in home instead of /Download/
storedInHome :: FilePath -> Bool
storedInHome file
    | file == "bonuschars.kart" = True
    | drop (length file - 3) file `elem` ["cfg","soc"] = True
    | otherwise = False




isCFG :: FilePath -> Bool
isCFG file = drop (length file - 3) file == "cfg"
    
    
    