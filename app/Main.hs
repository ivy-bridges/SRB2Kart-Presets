

module Main where

import Data.List
import Data.Either

import System.Environment
import System.Directory
import System.IO
import Control.Monad

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZ

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

main :: IO ()
main = do
    modlist:args <- getArgs -- get name of cfg file
    
    tarball <- packCFG modlist
    
    let tarLocation = take (length modlist - 4) modlist ++ ".tar.gz"
    
    BS.writeFile tarLocation tarball
    
    
    unpackPreset tarLocation


-- packs files from a cfg into a compressed tar.gz file
packCFG :: FilePath -> IO BS.ByteString
packCFG modlist = do
    paths <- readsettings
    
    let kartFolder = head paths
        dlFolder = subDirectory kartFolder "Download"
        
    -- move into kart's folder to grab list of files from cfg
    files <- withCurrentDirectory kartFolder (modfiles modlist) 
    
    -- some are stored in the home folder (cfgs, socs, bonuschars) while some are in downloads
    let (homeFiles, dlFiles) = partition storedInHome files 
    -- grabs the files from home
    -- and files from download
    homeEntries <- Tar.pack kartFolder homeFiles -- Tar.pack :: FilePath -> [FilePath] -> IO [Entry]
    dlEntries   <- Tar.pack dlFolder   dlFiles 
    
    
    -- toTarPath returns an Either value, where a Left denotes an error and a Right returns a tarPath
    -- i don't actually know how you're supposed to handle this Either when you can't construct a default tarPath value    
    -- so it throws an error instead
    let infoPath      = either (error "Invalid Path") (id) (Tar.toTarPath False (modlist++".info"))
        infoEntry     = Tar.fileEntry infoPath (packString modlist)
        
        fullArchive   = infoEntry:(homeEntries ++ dlEntries) -- combine and compress the archive
        compressedTar = (GZ.compress . Tar.write) fullArchive
    
    return (compressedTar)
 
-- given a compressed preset, decompress and read the contents
-- TODO : copy files that don't already exist, and create a .bat file to launch the game with this preset
unpackPreset :: FilePath -> IO ()
unpackPreset filename = do
    paths <- readsettings
    let kartFolder = head paths
    
    -- move into kart's folder to read the file
    contents <- withCurrentDirectory kartFolder (BS.readFile filename)
    
        
    let decompressed = GZ.decompress contents
        tarEntries = Tar.read decompressed
        
        -- accumulate the entryPath of each entry, ignoring errors
        names = Tar.foldEntries ((:) . Tar.entryPath) [] (\e -> []) tarEntries
        infoName = filter (isSuffixOf ".info") names
        
    print names
    print infoName
    

-- packs a string to a bytestring
-- this is used to store the name of the main cfg in the compressed file
packString :: String -> BS.ByteString
packString = encodeUtf8 . T.pack
    
    
    

    


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
    
    
    