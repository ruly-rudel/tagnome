module TAGnome.Files
  (
      listFilesRecursive
  ) where

import System.Directory
import Data.List
import Control.Monad.IO.Class
import UnliftIO


{-
listFilesRecursive :: MonadIO m => [FilePath] -> m [FilePath]
listFilesRecursive [] = return []
listFilesRecursive (x:xs) = do
  e <- (liftIO . doesDirectoryExist) x
  if e then do
    files <- (liftIO . listDirectory) x
    rx  <- listFilesRecursive (sort (map ((x ++ "/") ++) files))
    rxs <- listFilesRecursive xs
    return $ rx ++ rxs
  else do
    rxs <- listFilesRecursive xs
    return $ x : rxs
-}

listFilesRecursive :: MonadIO m => [FilePath] -> m [FilePath]
listFilesRecursive = fmap concat . traverse listFilesRecursive1
{-
listFilesRecursive lst = do
    r <- traverse listFilesRecursive1 lst
    return $ concat r
-}


listFilesRecursive1 :: MonadIO m => FilePath -> m [FilePath]
listFilesRecursive1 path = do
  e <- (liftIO . doesDirectoryExist) path
  if e then do 
    list <- (liftIO . listDirectory) path
    listFilesRecursive (sort (map ((path ++ "/") ++) list))
  else return [path]

