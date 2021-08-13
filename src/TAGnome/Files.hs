{-# LANGUAGE TupleSections #-}
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


{-
listFilesRecursive :: MonadIO m => [FilePath] -> m [FilePath]
listFilesRecursive [] = return []
listFilesRecursive (x:xs) = do
  e <- doesDirectoryExist x
  if e then do
    files <- listDirectory x
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
  e <- doesDirectoryExist path
  if e then do
    list <- listDirectory path
    listFilesRecursive (sort (map ((path ++ "/") ++) list))
  else return [path]



data MetaData = MetaInt String Int | MetaStr String String deriving (Eq, Show)

data FlacStream = FlacStream C.ByteString Int [MetaData]

newtype FP a = FP { runFP :: FlacStream -> (a, FlacStream)}

getStrByte :: Int -> C.ByteString -> String
getStrByte n bs = map (chr . fromEnum) $ C.unpack $ C.take n bs

getMetaStrByte :: Int -> C.ByteString  -> MetaData
getMetaStrByte n bs =
  let str = C.take n bs

      key = map (chr .fromEnum) $ C.unpack $ C.takeWhile ('='/=) str
      val = map (chr .fromEnum) $ C.unpack $ C.tail $ C.dropWhile ('='/=) str in
        MetaStr key val

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

parseStr :: Int -> Maybe String -> FP String
parseStr size key  = FP $ \(FlacStream bs pos meta) ->
  if size > 1024 then
    error "field size exceeds 1kbyte."
  else
    let str = getStrByte size $ B.drop pos bs in
      case key of
        Just k  -> (str, FlacStream bs (pos + size) (meta ++ [MetaStr k str]))
        Nothing -> (str, FlacStream bs (pos + size) meta)

parseMetaStr :: Int -> FP String
parseMetaStr size = FP $ \(FlacStream bs pos meta) ->
  if size > 1024 then
    error "field size exceeds 1kbyte."
  else
    let str = getMetaStrByte size $ B.drop pos bs in
      case str of
        MetaStr key val -> (val, FlacStream bs (pos + size) (meta ++ [str]))
        MetaInt _ _     -> ([],  FlacStream bs pos meta)




parseNum :: Int -> Maybe String -> FP Int
parseNum size key  = FP $ \(FlacStream bs pos meta) ->
  let num = getNumByte size $ C.drop pos bs in
    case key of
      Just k  -> (num, FlacStream bs (pos + size) (meta ++ [MetaInt k num]))
      Nothing -> (num, FlacStream bs (pos + size)  meta)

parseNumLE :: Int -> Maybe String -> FP Int
parseNumLE size key  = FP $ \(FlacStream bs pos meta) ->
  let num = getNumByteLE size $ C.drop pos bs in
    case key of
      Just k  -> (num, FlacStream bs (pos + size) (meta ++ [MetaInt k num]))
      Nothing -> (num, FlacStream bs (pos + size)  meta)


searchFlacVorbisComment :: FP Bool
searchFlacVorbisComment = do
  block_type   <- parseNum 1 $ Just "BLOCK_TYPE"
  block_length <- parseNum 3 $ Just "BLOCK_LENGTH"
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
