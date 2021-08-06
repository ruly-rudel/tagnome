module TAGnome.Files
  (
      listFilesRecursive
    , listFilesRecursive1
  ) where

import System.Directory
import Data.List
import Control.Monad


listFilesRecursive :: [FilePath] -> IO [FilePath]
listFilesRecursive lst = do
    r <- traverse listFilesRecursive1 lst
    return $ concat r 

listFilesRecursive1 :: FilePath -> IO [FilePath]
listFilesRecursive1 path = do
  e <- doesDirectoryExist path
  if e then do 
    list <- listDirectory path
    listFilesRecursive (sort (map ((path ++ "/") ++) list))
  else do
    f <- doesFileExist path
    if f then 
      return [path]
    else
      return []


{-
listFilesRecursive :: [FilePath] -> IO (Maybe [FilePath])
listFilesRecursive lst = do
  traverse listFilesRecursive1 lst


listFilesRecursive1 :: FilePath -> IO (Maybe [FilePath])
listFilesRecursive1 path = do
  e <- doesDirectoryExist path
  if e then (do 
    list <- listDirectory path
    listFilesRecursive (sort list)
    )
  else return $ Just []    

-}


{-
listFilesRecursive1 :: FilePath -> IO (Maybe [FilePath])
listFilesRecursive1 path = do
  e <- doesDirectoryExist path
  if e then (do 
    list <- listDirectory path
    listFilesRecursive (sort list)
    )   
  else (do
    f <- doesFileExist path
    if f then
      return $ Just [path]
    else
      return Nothing
  )
-}

