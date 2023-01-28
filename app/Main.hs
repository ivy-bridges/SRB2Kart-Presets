

module Main where

import Data.List

import System.Environment
import System.Directory
import System.IO
import Control.Monad

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZ




-- packs files from a cfg into a compressed tar folder
packCFG :: FilePath -> IO ()
packCFG modlist = do
    paths <- readsettings
    
    let kartFolder = head paths
        dlFolder = subDirectory kartFolder "Download"
        
        -- chop off the .cfg extension and replace with .tar
        tarDestination = take (length modlist - 4) modlist ++ ".tar.gz"
    
   
    
    setCurrentDirectory kartFolder -- move to kart folder
    files <- modfiles modlist      -- and grab the list of files from the cfg
    
    -- some are stored in the home folder (cfgs, socs, bonuschars) while some are in downloads
    let (homeFiles, dlFiles) = partition storedInHome files 
    
    
    
    -- grabs the files from home
    -- and files from download
    homeEntries <- Tar.pack kartFolder homeFiles -- Tar.pack :: FilePath -> [FilePath] -> IO [Entry]
    dlEntries   <- Tar.pack dlFolder   dlFiles 
    
    
    let fullArchive = homeEntries ++ dlEntries
        compressedTar = (GZ.compress . Tar.write) fullArchive
    
    putStrLn "Packing .tar archive..."
    BS.writeFile tarDestination compressedTar
    
    
    
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
    putStrLn ("Grabbing filenames from " ++ filename ++ "...")
    contents <- readFile filename
    let commands = lines contents  
        execs = filter (\i -> take 4 i == "exec") commands -- only lines starting with exec. these denote additional cfg files
        cfgs = map (drop 5) execs -- just get the filename
        
        addfiles = filter (\i -> take 7 i == "addfile") commands -- only lines starting with "addfile"
        files = cfgs ++ (map (drop 8) addfiles) -- just get the filename
        
        runsocs = filter (\i -> take 6 i == "runsoc") commands -- only lines starting with runsoc. these denote soc files
        socs = map (drop 7) runsocs -- just get the file name
    
    -- recurses into referenced cfg files and adds them to mods list
    additionals <- mapM modfiles cfgs
    
    return (nub $ concat [files,socs, concat additionals]) -- combine the additionals with the base list and wrap them for IO
    
    


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
    
    
    