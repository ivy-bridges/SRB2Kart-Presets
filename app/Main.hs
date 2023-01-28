

module Main where

import Data.List

import System.Environment
import System.Directory
import System.IO
import Control.Monad

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZ

main :: IO ()
main = do
    modlist:args <- getArgs -- get name of cfg file
    
    packCFG modlist


-- packs files from a cfg into a compressed tar folder
packCFG :: FilePath -> IO ()
packCFG modlist = do
    paths <- readsettings
    
    let kartFolder = head paths
        dlFolder = subDirectory kartFolder "Download"
        -- chop off the .cfg extension and replace with .tar.gz
        tarDestination = take (length modlist - 4) modlist ++ ".tar.gz"
    
   
    
    setCurrentDirectory kartFolder -- move to kart folder
    files <- modfiles modlist      -- and grab the list of files from the cfg
    
    -- some are stored in the home folder (cfgs, socs, bonuschars) while some are in downloads
    let (homeFiles, dlFiles) = partition storedInHome files 
    -- grabs the files from home
    -- and files from download
    homeEntries <- Tar.pack kartFolder homeFiles -- Tar.pack :: FilePath -> [FilePath] -> IO [Entry]
    dlEntries   <- Tar.pack dlFolder   dlFiles 
    
    let fullArchive = homeEntries ++ dlEntries -- combine and compress the archive
        compressedTar = (GZ.compress . Tar.write) fullArchive
    
    putStrLn "Packing .tar archive..."
    BS.writeFile tarDestination compressedTar
    
    
    

    


-- grabs the destination from settings.txt
readsettings :: IO [String]
readsettings = do
    contents <- readFile "settings.txt"
    return (lines contents)

-- grab cfgs and addons from a cfg file
modfiles :: FilePath -> IO [String]
modfiles filename = do
    putStrLn ("Grabbing filenames from " ++ filename ++ "...")
    contents <- readFile filename
    let commands = lines contents
        
        -- we need the cfg and soc files as well as any mods it adds
        cfgs     = grabCommands "exec" commands        
        socs     = grabCommands "runsoc" commands
        addfiles = grabCommands "addfile" commands
        
        files = cfgs ++ socs ++ addfiles
    
    -- recurses into referenced cfg files and adds them to mods list
    additionals <- mapM modfiles cfgs
    -- combine the additionals with the base list and wrap them for IO, removing duplicates
    return (nub $ concat [files,socs, concat additionals]) 
    


-- convenience functions

-- cfg, soc, and the bonuschars.kart files are all stored in home instead of /Download/
storedInHome :: FilePath -> Bool
storedInHome file
    | file == "bonuschars.kart" = True
    | drop (length file - 3) file `elem` ["cfg","soc"] = True
    | otherwise = False
    


-- given a prefix to match and a list of commands, returns all matching lines with the prefix removed
grabCommands :: String -> [String] -> [String]
grabCommands prefix commands = map (drop $ length prefix + 1) matches
    where matches = filter (isPrefixOf prefix) commands


-- i wrote this because i am sick of writing ++"/" to piece together folders manually
subDirectory :: FilePath -> String -> FilePath
subDirectory main sub = main ++ "/" ++ sub
    
    
    