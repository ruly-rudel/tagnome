{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TAGnome.Files
  (
      listFilesRecursive
     ,FilePathEx(FilePathEx)
     ,getFlacMetadataFromFile
     ,MetaData(MetaFile, MetaInt, MetaStr)
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


data MetaData = MetaFile String FilePathEx | MetaInt String Int | MetaStr String T.Text  deriving (Eq, Show)

data FlacStream = FlacStream C.ByteString Int [MetaData]

newtype FP a = FP { runFP :: FlacStream -> (a, FlacStream)}

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

lastn :: Int -> [Char] -> [Char]
lastn num str =
  let len = length str in
    if len < num then str else drop (len - num) str

endWith :: [Char] -> [Char] -> Bool
endWith suffix str =
  let len = length suffix in
    suffix == lastn len str

getFlacMetadataFromFile ::  FilePathEx -> IO (Maybe [MetaData])
getFlacMetadataFromFile (FilePathEx base path) = 
  if endWith ".flac" path then do 
    fe <- doesFileExist (base ++ path)
    if fe then do
      bs <- mmapFileByteString (base ++ path) Nothing
      return $ Just $ fst $ (`runFP` FlacStream bs 0 [MetaFile "orignal_file" $ FilePathEx base path ]) $ do
        magic <- parseStr 4 $ Just "MAGIC"
        if magic /= "fLaC" then
          error $ base ++ path ++ ": cannot find a MAGIC of FLAC file."
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
    else return Nothing
  else return Nothing


