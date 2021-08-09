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

data FlacStream = FlacStream ByteString Int [MetaData] Int String

nextFlacStream :: Int -> FlacStream -> FlacStream
nextFlacStream size fs = case fs of
  FlacStream bs pos meta i s -> FlacStream bs (pos + size) meta i s

parseNum :: String -> Int -> FlacStream -> FlacStream
parseNum key size fs = case fs of
    FlacStream bs pos meta i s -> 
      let num = (getNumByte size $ B.drop pos bs) in
        FlacStream bs (pos + size) (meta ++ [MetaInt key num]) num s

parseNumLE :: String -> Int -> FlacStream -> FlacStream
parseNumLE key size fs = case fs of
  FlacStream bs pos meta i s -> 
      let num = (getNumByteLE size $ B.drop pos bs) in
        FlacStream bs (pos + size) (meta ++ [MetaInt key num]) num s

parseStr :: String -> Int -> FlacStream ->  FlacStream
parseStr key size fs = case fs of
  FlacStream bs pos meta i s -> 
    let str = getStrByte size $ B.drop pos bs in
      FlacStream bs (pos + size) (meta ++ [MetaStr key str]) i str

lastMetaIntOf :: FlacStream -> Int
lastMetaIntOf fs = case fs of
  FlacStream _ _ _ i _-> i

getMeta :: FlacStream -> [MetaData]
getMeta fs = case fs of
  FlacStream _ _ meta _ _-> meta

(-:-) :: FlacStream -> (FlacStream -> a) -> a
x -:- y = y x
infixl -:-



getFlacMetadataFromFile ::  FilePath -> IO [MetaData]
getFlacMetadataFromFile path = do
  bs <- mmapFileByteString path Nothing
  let fs1 = FlacStream bs 0 [] 0 []
        -:- parseStr "MAGIC" 4
        -:- parseNum "BLOCK_TYPE.0" 1
        -:- parseNum "LENGTH.0" 3 in
          let fs2 = fs1
               -:- nextFlacStream (lastMetaIntOf fs1)
               -:- parseNum "BLOCK_TYPE.1" 1
               -:- parseNum "LENGTH.1" 3
               -:- parseNumLE "vender_length" 4 in
                 let fs3 = fs2
                      -:- parseStr "vender_string" (lastMetaIntOf fs2)
                      -:- parseNumLE "user_comment_list_length" 4
                      -:- parseNumLE "comment#00 length" 4 in
                        return $ fs3
                               -:- parseStr "comment#00" (lastMetaIntOf fs3)
                               -:- getMeta


                

          

{-  
  let fs = FlacStream bs 0 [] in
    let fs1 = parseStr "MAGIC" 4 fs in
      let fs2 = parseNum "BLOCK_TYPE.0" 1 fs1 in
        let fs3 = parseNum "LENGTH.0" 3 fs2 in
                  let fs4 = nextFlacStream (lastMetaIntOf fs3) fs3 in
                    let fs5 = parseNum "BLOCK_TYPE.1" 1 fs4 in
                      let fs6 = parseNum "LENGTH.1" 3 fs5 in
                        let fs7 = parseNumLE "vender_length" 4 fs6 in
                          case fs7 of
                            FlacStream _ _ meta -> return meta

-}

  {-
  return [MetaStr "MAGIC" $ getStrByte 4 bs,
          MetaInt "BLOCK_TYPE.1" $ getNumByte 1 $ B.drop 4 bs,
          MetaInt "Length" $ getNumByte 3 $ B.drop 5 bs,
          MetaInt "BLOCK_TYPE.2" $ getNumByte 1 $ B.drop (8 + 34) bs,
          MetaInt "Length" $ getNumByte 3 $ B.drop (8 + 34 + 1) bs,
          MetaInt "vender_length" $ getNumByteLE 4 $ B.drop (8 + 34 + 4) bs,
          MetaStr "vender_string" $ getStrByte 13 $ B.drop (8 + 34 + 8) bs,
          MetaInt "user_comment_list_length" $ getNumByteLE 4 $ B.drop (8 + 34 + 8 + 13) bs,
          MetaInt "COMMENT#00 Length" $ getNumByteLE 4 $ B.drop (8 + 34 + 8 + 13 + 4) bs,
          MetaStr "COMMENT#00" $ getStrByte 15 $ B.drop (8 + 34 + 8 + 13 + 8) bs,
          MetaInt "COMMENT#01 Length" $ getNumByteLE 4 $ B.drop (8 + 34 + 8 + 13 + 8 + 15) bs,
          MetaStr "COMMENT#01" $ getStrByte 20 $ B.drop (8 + 34 + 8 + 13 + 8 + 15 + 4) bs
         ]
-}
  