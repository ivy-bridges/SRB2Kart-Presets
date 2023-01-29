

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

import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

main :: IO ()
main = do
    modlist:args <- getArgs -- get name of cfg file
    paths <- readsettings
    
    
    
    
    let tarLocation = take (length modlist - 4) modlist ++ ".tgz"
        kartFolder = head paths
    
    unpackPreset tarLocation
    
    --unpackPreset tarLocation


-- packs files from a cfg into a compressed tar.gz file
packCFG :: FilePath -> IO BS.ByteString
packCFG modlist = do
    paths <- readsettings
    
    let kartFolder = head paths
        dlFolder = subDirectory kartFolder "Download"
        
        cfgName = take (length modlist - 4) modlist
        presetFolder = subDirectory dlFolder cfgName
        
    -- move into kart's folder to grab list of files from cfg
    files <- withCurrentDirectory kartFolder (modfiles modlist) 
    
    -- some are stored in the home folder (cfgs, socs, bonuschars) while some are in downloads
    let (homeFiles, dlFiles) = partition storedInHome files
    
    -- we check the downloads folder as well as the preset folder for non-home files
    inPreset    <- withCurrentDirectory (presetFolder) (filterM doesFileExist dlFiles)
    inDownloads <- withCurrentDirectory (dlFolder)     (filterM doesFileExist dlFiles)
    -- doesFileExist returns False for invalid directories (when no preset exists)
    -- in that case, presetEntries is the empty list []
    
    print inPreset
    print inDownloads
    
    -- grabs the files from home
    -- and files from download
    -- also tries to grab files from preset folder if that exists
    homeEntries   <- Tar.pack kartFolder   homeFiles -- Tar.pack :: FilePath -> [FilePath] -> IO [Entry]
    dlEntries     <- Tar.pack dlFolder     inDownloads
    presetEntries <- Tar.pack presetFolder inPreset
    
    -- toTarPath returns an Either value, where a Left denotes an error and a Right returns a tarPath
    -- i don't actually know how you're supposed to handle this Either when you can't construct a default tarPath value    
    -- so it throws an error instead
    let infoPath      = either (error "Invalid Path") (id) (Tar.toTarPath False ("preset.info"))
        infoEntry     = Tar.fileEntry infoPath (packString modlist)
        
        fullArchive   = infoEntry:(homeEntries ++ dlEntries ++ presetEntries) -- combine and compress the archive
        compressedTar = (GZ.compress . Tar.write) fullArchive
    
    return (compressedTar)
 
-- given a compressed preset, decompress and read the contents
-- copies files that don't already exist, and creates a launcher
unpackPreset :: FilePath -> IO ()
unpackPreset filename = do
    paths <- readsettings
    let kartFolder = head paths
    
    -- move into kart's folder to read the file
    contents <- withCurrentDirectory kartFolder (BS.readFile filename)
    
        
    let decompressed = GZ.decompress contents
        tarEntries = Tar.read decompressed
        
        -- the Tar package uses a special Entries datatype
        -- accumulate the individual entries into an [Entry], ignoring errors
        entryList = Tar.foldEntries (:) [] (\e -> []) tarEntries
        
        entryNames = map (Tar.entryPath) entryList
        entryContents = map (grabContents . Tar.entryContent) entryList
        
        -- find the file containing preset information
        infoName = filter (isSuffixOf ".info") entryNames
        
        -- split the info entry off from the rest
        entries = zip entryNames entryContents
        (infoEntries, dataEntries) = partition (\(f,c) -> ".info" `isSuffixOf` f) entries
        
        -- read the info entry to get the name of the preset
        presetFile = (Text.unpack . decodeUtf8) $ (snd . head) infoEntries
        presetName = take (length presetFile - 4) presetFile
        
        launcherFile = "Launch with " ++ presetName ++ ".bat"
    
        
    -- copy the files to the correct locations if necessary
    mapM_ (copymod presetName) dataEntries
    
    -- and create the launcher
    launcherString <- createLauncher presetFile
    withCurrentDirectory kartFolder (writeFile launcherFile launcherString)
    
    
    
    
    
-- creates a bat file to launch the game with the preset
createLauncher :: FilePath -> IO String
createLauncher cfgFile = do
    paths <- readsettings
    let kartLauncher = paths !! 1
        
        batchContents = "@echo off\nSTART " ++ kartLauncher ++ " +exec " ++ cfgFile
    
    return batchContents
      
    

-- given destination and a pair (filename, contents), copies contents to filename in destination if necessary
-- socs, cfgs, and bonuschars.kart go in the home
-- other files are placed in a folder with the cfg name
copymod :: FilePath -> (FilePath, BS.ByteString) -> IO ()
copymod destination (filename, contents) = do
    paths <- readsettings
    let kartFolder = head paths
        dlFolder = subDirectory kartFolder "Download"
    
    -- places to search for the file
    let searchLocations
            | storedInHome filename = [kartFolder]
            | otherwise = [subDirectory dlFolder destination, dlFolder]


    findResults <- findFiles searchLocations filename
    let needToCopy      = null findResults -- if none are found, we need to copy the file
        copyDestination = subDirectory (head searchLocations) filename   -- into the correct destination
    --putStrLn mod
    --print (storedInHome mod)

    
    when needToCopy $ withCurrentDirectory kartFolder (do
        createDirectoryIfMissing False (head searchLocations)
        BS.writeFile copyDestination contents)

-- packs a string to a bytestring
-- this is used to store the name of the main cfg in the compressed file
packString :: String -> BS.ByteString
packString = encodeUtf8 . Text.pack
    
    
-- grab the contents from a tar archive entry
-- im not sure if matching against the constructor like this is good form. but it works
grabContents :: Tar.EntryContent -> BS.ByteString
grabContents (Tar.NormalFile c _) = c
grabContents _ = BS.empty

    


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
        
        -- we also grab the cfg in question
        files = filename:(cfgs ++ socs ++ addfiles)
    
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


    
    
    