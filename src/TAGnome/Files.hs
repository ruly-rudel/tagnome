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
import System.IO.MMap
import Data.Char
import Control.Applicative
import Control.Monad

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

getStrByte :: Int -> B.ByteString -> String
getStrByte n bs = map (chr . fromEnum) $ B.unpack $ B.take n bs

getNumByte :: Int -> B.ByteString -> Int
getNumByte n bs = foldl (\x y -> x * 256 + fromEnum y) 0 $ B.unpack $ B.take n bs

getNumByteLE :: Int -> B.ByteString -> Int
getNumByteLE n bs = foldr (\x y -> fromEnum x + y * 256) 0 $ B.unpack $ B.take n bs

data MetaData = MetaInt String Int | MetaStr String String deriving (Eq, Show)

data FlacStream = FlacStream B.ByteString Int [MetaData]

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

parseNext :: Int -> FP ()
parseNext size = FP $ \(FlacStream bs pos meta) -> ((), FlacStream bs (pos + size) meta)

parseStr :: String -> Int -> FP String
parseStr key size  = FP $ \(FlacStream bs pos meta) ->
  let str = getStrByte size $ B.drop pos bs in
    (str, FlacStream bs (pos + size) (meta ++ [MetaStr key str]))

parseNum :: String -> Int -> FP Int
parseNum key size  = FP $ \(FlacStream bs pos meta) ->
  let num = getNumByte size $ B.drop pos bs in
    (num, FlacStream bs (pos + size) (meta ++ [MetaInt key num]))

parseNumLE :: String -> Int -> FP Int
parseNumLE key size  = FP $ \(FlacStream bs pos meta) ->
  let num = getNumByteLE size $ B.drop pos bs in
    (num, FlacStream bs (pos + size) (meta ++ [MetaInt key num]))

getFlacMetadataFromFile ::  FilePath -> IO [MetaData]
getFlacMetadataFromFile path = do
  bs <- mmapFileByteString path Nothing
  return $ fst $ runFP ( do
        parseStr   "MAGIC" 4
        parseNum   "BLOCK_TYPE.0" 1
        len0 <- parseNum   "Length" 3
        parseNext len0
        parseNum   "BLOCK_TYPE.1" 1
        parseNum   "Length" 3
        vlen <- parseNumLE "vender_length" 4
        parseStr   "vender_string" vlen
        clen <- parseNumLE "user_comment_list_length" 4
        forM_ [1..clen] $ \x -> parseNumLE ("comment len #" ++ show x) 4 >>= parseStr ("comment#" ++ show x)
        getMeta
    ) (FlacStream bs 0 [])
