module TAGnome.Files
  (
      listFilesRecursive
    , listFilesRecursive1
  ) where

import System.Directory
import Data.List
import Control.Monad


listFilesRecursive :: [FilePath] -> IO (Maybe [FilePath])
listFilesRecursive lst = do
    r <- traverse listFilesRecursive1 lst
    let rr = sequence r in
      return $ case rr of
        Nothing -> Nothing
        Just a  -> Just $ concat a


listFilesRecursive1 :: FilePath -> IO (Maybe [FilePath])
listFilesRecursive1 path = do
  e <- doesDirectoryExist path
  if e then do 
    list <- listDirectory path
    listFilesRecursive (sort (map ((path ++ "/") ++) list))
  else do
    f <- doesFileExist path
    if f then 
      return $ Just [path]
    else
      return Nothing
