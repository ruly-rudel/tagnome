{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TAGnome.Files
  (
      listFilesRecursive
     ,FilePathEx(FilePathEx)
     ,getFlacMetadataFromFile
     ,MetaData(MetaInt, MetaStr)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.List
import Data.String.Conversions
import Data.Typeable
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import System.IO.MMap
import UnliftIO.Directory 


data FilePathEx = FilePathEx FilePath FilePath deriving (Eq, Ord, Show)

listFilesRecursive :: MonadIO m => [FilePathEx] -> m [FilePathEx]
listFilesRecursive = fmap concat . traverse listFilesRecursive1

listFilesRecursive1 :: MonadIO m => FilePathEx -> m [FilePathEx]
listFilesRecursive1 (FilePathEx base path) = do
  e <- doesDirectoryExist $ base ++ path
  if e then do
    list <- listDirectory $ base ++ path
    listFilesRecursive $ sort (map (\x -> FilePathEx base $ path ++ "/" ++ x) list)
  else return [FilePathEx base path]

data TnExceptions = SuffixNotSupportedException | FlacFileNotFoundException | IsNotFlacFileException | FlacVorbisCommentNotFoundException
  deriving (Show, Eq, Typeable)

instance Exception TnExceptions

data MetaData = MetaInt String Int | MetaStr String T.Text  deriving (Eq, Show)

lastn :: Int -> [a] -> [a]
lastn num str =
  let len = length str in
    if len < num then str else drop (len - num) str

endWith :: Eq a => [a] -> [a] -> Bool
endWith suffix str =
  suffix == lastn (length suffix) str

skipByte :: (MonadState C.ByteString m) => Int -> m C.ByteString
skipByte n = do
  bs <- get
  put $ C.drop n bs
  return bs

getByte :: (MonadState C.ByteString m) => Int -> m C.ByteString
getByte n = do
  bs <- skipByte n
  return $ C.take n bs

getStrByte :: (MonadState C.ByteString m) => Int -> m [Char]
getStrByte n = convertString <$> getByte n

getPairStrByte :: (MonadState C.ByteString m) => Int -> m ([Char], T.Text)
getPairStrByte n = do
  str <- getByte n
  let key = convertString $ C.takeWhile ('='/=) str
      val = convertString $ C.tail $ C.dropWhile ('='/=) str in
        return (key, val)

getNumByteBE :: (MonadState C.ByteString m) => Int -> m Int
getNumByteBE n = do
  str <- getByte n
  return $ foldl (\x y -> x * 256 + fromEnum y) 0 $ C.unpack str

getNumByteLE :: (MonadState C.ByteString m) => Int -> m Int
getNumByteLE n = do
  str <- getByte n
  return $ foldr (\x y -> fromEnum x + y * 256) 0 $ C.unpack str

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb ma = do
  r <- mb
  unless r ma

parseMagic :: (MonadState C.ByteString m, MonadThrow m) => m [Char]
parseMagic = do
  magic <- getStrByte 4
  if magic == "fLaC" then
    return magic
  else
    throwM IsNotFlacFileException

skipToVorbisComment :: (MonadState C.ByteString m, MonadThrow m) => m Int
skipToVorbisComment = do
  block_type <- getNumByteBE 1
  block_length <- getNumByteBE 3
  if block_type == 4 || block_type == (128 + 4) then
    return block_type
  else do
    if block_type > 127 then
      throwM FlacVorbisCommentNotFoundException
    else do
      skipByte block_length
      skipToVorbisComment

parseVorbisComment :: (MonadState C.ByteString m, MonadThrow m) => m [MetaData]
parseVorbisComment = do
  vender_length <- getNumByteLE 4
  vender_string <- getStrByte vender_length
  user_comment_list_length <- getNumByteLE 4
  forM [1..user_comment_list_length] $ \x -> do
    cl <- getNumByteLE 4
    (key, val) <- getPairStrByte cl
    return $ MetaStr key val

getFlacMetadataFromFile :: (MonadIO m, MonadThrow m) => FilePathEx -> m [MetaData]
getFlacMetadataFromFile (FilePathEx base path) = do
  unless  (endWith ".flac" fullpath) $ throwM SuffixNotSupportedException
  unlessM (doesFileExist   fullpath) $ throwM FlacFileNotFoundException
  bs <- liftIO $ mmapFileByteString fullpath Nothing
  (`evalStateT` bs) $ do
    parseMagic
    skipToVorbisComment
    parseVorbisComment
  where
    fullpath = base ++ path

