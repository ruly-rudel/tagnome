{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Text.Show.Unicode
import Control.Monad ( void )
import TAGnome.Files


main :: IO ()
main = do
    meta <- getFlacMetadataFromFile "./tv/01/tone2.flac"
    uprint meta
    void $ runTestTT $ TestList
      [
{-
         "00_00_listFilesRecursive1" ~: (do
          e <- listFilesRecursive1 "./tv/00/notexist"
          assertEqual "00_00 dir not exists." Nothing e)

        ,"00_01_listFilesRecursive1" ~: (do
          e <- listFilesRecursive1 "./tv/00/file1"
          assertEqual "00_01 not a dir." (Just ["./tv/00/file1"]) e)

        ,"00_02_listFilesRecursive1" ~: (do
          e <- listFilesRecursive1 "./tv/00"
          assertEqual "00_02 normal 01." (Just ["./tv/00/dir1/file3", "./tv/00/dir1/file4", "./tv/00/file1", "./tv/00/file2"]) e)
          
        ,"00_03_listFilesRecursive1" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir1"
          assertEqual "00_03 normal 02." (Just ["./tv/00/dir1/file3", "./tv/00/dir1/file4"]) e)

        ,"00_04_listFilesRecursive1" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir2/dir3"
          assertEqual "00_04 empty dir." (Just []) e)
          
        ,"00_05_listFilesRecursive1" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir2/"
          assertEqual "00_05 only single dir." (Just []) e)
-}

         "00_06_listFilesRecursive" ~: (do
          e <- listFilesRecursive ["./tv/00/notexist"]
          assertEqual "00_06 dir not exists." ["./tv/00/notexist"] e)

        ,"00_07_listFilesRecursive" ~: (do
          e <- listFilesRecursive ["./tv/00/file1"]
          assertEqual "00_07 not a dir." ["./tv/00/file1"] e)

        ,"00_08_listFilesRecursive" ~: (do
          e <- listFilesRecursive ["./tv/00"]
          assertEqual "00_08 normal 01." ["./tv/00/dir1/file3", "./tv/00/dir1/file4", "./tv/00/file1", "./tv/00/file2"] e)
          
        ,"00_09_listFilesRecursive" ~: (do
          e <- listFilesRecursive ["./tv/00/dir1"]
          assertEqual "00_09 normal 02." ["./tv/00/dir1/file3", "./tv/00/dir1/file4"] e)

        ,"00_10_listFilesRecursive" ~: (do
          e <- listFilesRecursive ["./tv/00/dir2/dir3"]
          assertEqual "00_10 empty dir." [] e)
          
        ,"00_11_listFilesRecursive" ~: (do
          e <- listFilesRecursive ["./tv/00/dir2/"]
          assertEqual "00_11 only single dir." [] e)
          
        ,"00_12_listFilesRecursive" ~: (do
          e <- listFilesRecursive ["./tv/00/dir1", "./tv/00/dir2/"]
          assertEqual "00_12 multi delectories search." ["./tv/00/dir1/file3", "./tv/00/dir1/file4"] e)


        ,"01_00_getFlacMetadataFromFile" ~: (do
          e <- getFlacMetadataFromFile "./tv/01/tone1.flac"
          assertEqual "01_00 flac matadata retrival." [MetaStr "MAGIC" "fLaC",MetaInt "BLOCK_TYPE" 0,MetaInt "BLOCK_LENGTH" 34,MetaInt "BLOCK_TYPE" 4,MetaInt "BLOCK_LENGTH" 125,MetaStr "vender_string" "Lavf56.40.101",MetaInt "user_comment_list_length" 8,MetaStr "date" "",MetaStr "artist" "",MetaStr "title" "",MetaStr "genre" "",MetaStr "DESCRIPTION" "",MetaStr "IENG" "",MetaStr "copyright" "",MetaStr "encoder" "Lavf56.40.101"]  e)


        ,"01_01_getFlacMetadataFromFile" ~: (do
          e <- getFlacMetadataFromFile "./tv/01/tone2.flac"
          assertEqual "01_01 flac matadata retrival." [MetaStr "MAGIC" "fLaC",MetaInt "BLOCK_TYPE" 0,MetaInt "BLOCK_LENGTH" 34,MetaInt "BLOCK_TYPE" 4,MetaInt "BLOCK_LENGTH" 553,MetaStr "vender_string" "Lavf56.40.101",MetaInt "user_comment_list_length" 19,MetaStr "DATE" "2021-01-01",MetaStr "ARTIST" "sample artist",MetaStr "TITLE" "sample title",MetaStr "GENRE" "sample genre",MetaStr "DESCRIPTION" "",MetaStr "IENG" "",MetaStr "copyright" "",MetaStr "encoder" "Lavf56.40.101",MetaStr "DISCTOTAL" "2",MetaStr "TOTALTRACKS" "10",MetaStr "DISCNUMBER" "1",MetaStr "TITLESORT" "ｓａｍｐｌｅ　ｔｉｔｌｅ",MetaStr "ALBUMSORT" "ｓａｍｐｌｅ　ａｌｂｕｍ",MetaStr "COMPOSER" "sample creator",MetaStr "ALBUM" "sample album",MetaStr "TRACKNUMBER" "2",MetaStr "ALBUMARTIST" "sample album artist",MetaStr "ALBUMARTISTSORT" "ｓａｍｐｌｅ　ａｌｂｕｍ　ａｒｔｉｓｔ",MetaStr "ARTISTSORT" "ｓａｍｐｌｅ　ａｒｔｉｓｔ"]  e)


      ]
