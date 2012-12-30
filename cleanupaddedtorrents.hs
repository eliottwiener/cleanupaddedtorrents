import Control.Monad
import Data.List.Split
import Data.List
import System.Directory
import System.FilePath

root = buildPath [slash ++ "home","e","Downloads"]

slash :: FilePath
slash = [pathSeparator]

buildPath :: [FilePath] -> FilePath
buildPath fs = intercalate slash fs

copyPair :: FilePath -> (FilePath, FilePath)
copyPair f = (buildPath [root,"unsorted", f]
           , buildPath [root,"sorted", "torrent", dropExtension f])

lastTwo :: [a] -> [a]
lastTwo [] = []
lastTwo (x:[]) = [x]
lastTwo (x:y:[]) = [x, y]
lastTwo l = lastTwo $ tail l

extensions :: FilePath -> [FilePath]
extensions f = splitOn [extSeparator] $ takeExtensions f

addedTorrentP :: FilePath -> Bool
addedTorrentP f = (lastTwo $ extensions f) == ["torrent", "added"]

main = do
  contents <- getDirectoryContents $ buildPath [root,"unsorted"]
  ats <- return $ filter addedTorrentP contents
  mapM_ (\ p -> renameFile (fst p) (snd p)) $ map copyPair ats
