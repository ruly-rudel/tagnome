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
import Data.Word
import Data.Char
import GHC.Generics (Meta(MetaSel))
import Data.ByteString (ByteString)

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

data FlacStream = FlacStream ByteString Int [MetaData]

newtype FP a = FP { runFP :: FlacStream -> (FlacStream, a)}

(-:-) :: FP a -> (a -> FP b) -> FP b
x -:- y = FP $ \s ->
  let (s', a) = runFP x s
    in runFP (y a) s'
infixl -:-

(-::-) :: FP a -> FP b -> FP b
x -::- y = FP $ \s ->
  let (s', _) = runFP x s
    in runFP y s'
infixl -::-

getMeta :: FP [MetaData]
getMeta =  FP $ \fs -> case fs of
  FlacStream bs pos meta -> (fs, meta)

parseNext :: Int -> FP ()
parseNext size = FP $ \(FlacStream bs pos meta) -> (FlacStream bs (pos + size) meta, ())

parseStr :: String -> Int -> FP String
parseStr key size  = FP $ \(FlacStream bs pos meta) ->
  let str = getStrByte size $ B.drop pos bs in
    (FlacStream bs (pos + size) (meta ++ [MetaStr key str]), str)

parseNum :: String -> Int -> FP Int
parseNum key size  = FP $ \(FlacStream bs pos meta) ->
  let num = getNumByte size $ B.drop pos bs in
    (FlacStream bs (pos + size) (meta ++ [MetaInt key num]), num)

parseNumLE :: String -> Int -> FP Int
parseNumLE key size  = FP $ \(FlacStream bs pos meta) ->
  let num = getNumByteLE size $ B.drop pos bs in
    (FlacStream bs (pos + size) (meta ++ [MetaInt key num]), num)

getFlacMetadataFromFile ::  FilePath -> IO [MetaData]
getFlacMetadataFromFile path = do
  bs <- mmapFileByteString path Nothing
  return $ snd $ runFP (
         parseStr   "MAGIC" 4
    -::- parseNum   "BLOCK_TYPE.0" 1
    -::- parseNum   "Length" 3
    -:-  parseNext
    -::- parseNum   "BLOCK_TYPE.1" 1
    -::- parseNum   "Length" 3
    -::- parseNumLE "vender_length" 4
    -:-  parseStr   "vender_string"
    -::- parseNumLE "user_comment_list_length" 4
    -::- parseNumLE  "comment#00 length" 4
    -:-  parseStr    "comment#00"
    -::- parseNumLE  "comment#01 length" 4
    -:-  parseStr    "comment#01"
    -::- parseNumLE  "comment#02 length" 4
    -:-  parseStr    "comment#02"
    -::- parseNumLE  "comment#03 length" 4
    -:-  parseStr    "comment#03"
    -::- parseNumLE  "comment#04 length" 4
    -:-  parseStr    "comment#04"
    -::- parseNumLE  "comment#05 length" 4
    -:-  parseStr    "comment#05"
    -::- parseNumLE  "comment#06 length" 4
    -:-  parseStr    "comment#06"
    -::- parseNumLE  "comment#07 length" 4
    -:-  parseStr    "comment#07"
    -::- parseNumLE  "comment#08 length" 4
    -:-  parseStr    "comment#08"
    -::- parseNumLE  "comment#09 length" 4
    -:-  parseStr    "comment#09"
    -::- parseNumLE  "comment#10 length" 4
    -:-  parseStr    "comment#10"
    -::- parseNumLE  "comment#11 length" 4
    -:-  parseStr    "comment#11"
    -::- getMeta
    ) (FlacStream bs 0 [])
