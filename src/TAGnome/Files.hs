{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TAGnome.Files
  (
      listFilesRecursive
     ,getFlacMetadataFromFile
     ,MetaData(MetaInt, MetaStr)
  ) where

import Data.List
import Control.Monad.IO.Class
import UnliftIO.Directory
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.IO.MMap
import Data.Char
import Control.Applicative
import Control.Monad
import Data.String.Conversions
import qualified Data.Text as T


listFilesRecursive :: MonadIO m => [FilePath] -> m [FilePath]
listFilesRecursive = fmap concat . traverse listFilesRecursive1

listFilesRecursive1 :: MonadIO m => FilePath -> m [FilePath]
listFilesRecursive1 path = do
  e <- doesDirectoryExist path
  if e then do
    list <- listDirectory path
    listFilesRecursive (sort (map ((path ++ "/") ++) list))
  else return [path]



data MetaData = MetaInt String Int | MetaStr String T.Text  deriving (Eq, Show)

data FlacStream = FlacStream C.ByteString Int [MetaData]

newtype FP a = FP { runFP :: FlacStream -> (a, FlacStream)}

getStrByte :: Int -> C.ByteString -> T.Text
getStrByte n bs = convertString $ C.take n bs

getMetaStrByte :: Int -> C.ByteString  -> (String, T.Text)
getMetaStrByte n bs =
  let str = C.take n bs
      key = convertString $ C.takeWhile ('='/=) str
      val = convertString $ C.tail $ C.dropWhile ('='/=) str in
        (key, val)

getNumByte :: Int -> C.ByteString -> Int
getNumByte n bs = foldl (\x y -> x * 256 + fromEnum y) 0 $ C.unpack $ C.take n bs

getNumByteLE :: Int -> C.ByteString -> Int
getNumByteLE n bs = foldr (\x y -> fromEnum x + y * 256) 0 $ C.unpack $ C.take n bs

instance Functor FP where
  fmap f a = FP $ \s ->
    let (a', s') = runFP a s
      in (f a', s')

instance Applicative FP where
  pure a = FP (a, )
  (<*>) = undefined

instance Monad FP where
  x >>= y = FP $ \s ->
    let (a, s') = runFP x s
      in runFP (y a) s'

  x >> y = FP $ \s ->
    let (_, s') = runFP x s
      in runFP y s'

getMeta :: FP [MetaData]
getMeta =  FP $ \fs -> case fs of
  FlacStream bs pos meta -> (meta, fs)

parseNext :: Int -> FP Int
parseNext size = FP $ \(FlacStream bs pos meta) -> (size, FlacStream bs (pos + size) meta)

parseStr :: Int -> Maybe String -> FP T.Text
parseStr size key 
  | size > 1024 = error "field size exceeds 1kbyte."
  | otherwise = FP $ \(FlacStream bs pos meta) ->
      let str = getStrByte size $ B.drop pos bs in
        (str, FlacStream bs (pos + size) $ case key of Just k -> meta ++ [MetaStr k str]; Nothing -> meta)

parseMetaStr :: Int -> FP T.Text
parseMetaStr size 
  | size > 1024 = error "filed size exceeds 1kbyte"
  | otherwise   = FP $ \(FlacStream bs pos meta) ->
      let (key, val) = getMetaStrByte size $ B.drop pos bs in
        (val, FlacStream bs (pos + size) (meta ++ [MetaStr key val]))

parseNum   :: Int -> Maybe String -> FP Int
parseNum   = updateMetaNum getNumByte

parseNumLE :: Int -> Maybe String -> FP Int
parseNumLE = updateMetaNum getNumByteLE

updateMetaNum :: (Int -> C.ByteString -> Int) -> Int -> Maybe String -> FP Int
updateMetaNum fn size key = FP $ \(FlacStream bs pos meta) ->
  let num = fn size $ C.drop pos bs in
    (num, FlacStream bs (pos + size) $ case key of Just k -> meta ++ [MetaInt k num]; Nothing -> meta )

searchFlacVorbisComment :: FP Bool
searchFlacVorbisComment = do
  block_type   <- parseNum 1 Nothing --  "BLOCK_TYPE"
  block_length <- parseNum 3 Nothing --  "BLOCK_LENGTH"
  if (block_type == 4) || (block_type == (128 + 4)) then do
    return True
  else do
    if block_type > 127 then do
      return False
    else do
      parseNext block_length
      searchFlacVorbisComment

getFlacMetadataFromFile ::  FilePath -> IO [MetaData]
getFlacMetadataFromFile path = do
  bs <- mmapFileByteString path Nothing
  return $ fst $ runFP ( do
    magic <- parseStr 4 $ Just "MAGIC"
    if magic /= "fLaC" then
      error $ path ++ ": cannot find a MAGIC of FLAC file."
    else do
      found <- searchFlacVorbisComment
      if found then do
        vlen <- parseNumLE 4 Nothing -- "vender_length"
        parseStr vlen $ Just "vender_string"
        clen <- parseNumLE 4 $ Just "user_comment_list_length"
        forM_ [1..clen] $ \x -> do
          cl <- parseNumLE 4 Nothing -- ("comment len #" ++ show x)
          parseMetaStr cl
        getMeta
      else do
        getMeta
    ) (FlacStream bs 0 [])
