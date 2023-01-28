

module Main where

import Data.List

import System.Environment
import System.Directory
import System.IO
import Control.Monad

import Codec.Archive.Tar




-- packs files from a cfg into a tar folder
packCFG :: FilePath -> IO ()
packCFG modlist = do
    paths <- readsettings
    
    let kartFolder = head paths
    let dlFolder = subDirectory kartFolder "Download"
    
    
    let destination = subDirectory dlFolder modlist
    let cfgdestination = subDirectory dlFolder "CFGs"
    
   
    
    setCurrentDirectory kartFolder
    
    
    files <- modfiles modlist
    
    
    let (homeFiles, dlFiles) = partition storedInHome files
    
    

    let tarball = (subDirectory kartFolder modlist) ++ ".tar"
    -- creates a tarball, grabs the files from home
    -- then appends the files from download
    create (tarball) kartFolder homeFiles
    append (tarball) dlFolder   dlFiles
    
    
    
main :: IO ()
main = do
    modlist:args <- getArgs -- get name of cfg file
    
    packCFG modlist
    


-- grabs the destination from settings.txt
readsettings :: IO [String]
readsettings = do
    contents <- readFile "settings.txt"
    
    
    let paths = lines contents
    
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
    
    return (nub $ foldl (++) files additionals) -- combine the additionals with the base list and wrap them for IO
    
    


copymod :: String -> String -> String -> IO ()
copymod home dir mod = do
    let copyDest = if isCFG mod 
                   then subDirectory dir ("CFGs/"++mod)
                   else subDirectory dir mod
    
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

-- i wrote this because i am sick of writing ++"/" to piece together folders manually
subDirectory :: FilePath -> String -> FilePath
subDirectory main sub = main ++ "/" ++ sub
    
    
    