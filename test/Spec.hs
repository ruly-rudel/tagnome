{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Text.Show.Unicode
import Control.Monad ( void )
import TAGnome.Files


main :: IO ()
main = do
    meta <- getFlacMetadataFromFile "/mnt/d/メディア/Music/CD/アニメ・ゲーム//DATE A LIVE/Date A Music Extension/01-Dead Or Alive.flac"
    uprint meta
    void $ runTestTT $ TestList
      [
         "00_06_listFilesRecursive" ~: (do
          e <- listFilesRecursive [FilePathEx "./tv/00/notexist" ""]
          assertEqual "00_06 dir not exists." [FilePathEx "./tv/00/notexist" ""] e)

        ,"00_07_listFilesRecursive" ~: (do
          e <- listFilesRecursive [FilePathEx "./tv/00/file1" ""]
          assertEqual "00_07 not a dir." [FilePathEx "./tv/00/file1" ""] e)

        ,"00_08_listFilesRecursive" ~: (do
          e <- listFilesRecursive [FilePathEx "./tv/00" ""]
          assertEqual "00_08 normal 01." [FilePathEx "./tv/00" "/dir1/file3", FilePathEx "./tv/00" "/dir1/file4", FilePathEx "./tv/00" "/file1", FilePathEx "./tv/00" "/file2"] e)
          
        ,"00_09_listFilesRecursive" ~: (do
          e <- listFilesRecursive [FilePathEx "./tv/00/dir1" ""]
          assertEqual "00_09 normal 02." [FilePathEx "./tv/00/dir1" "/file3", FilePathEx "./tv/00/dir1" "/file4"] e)

        ,"00_10_listFilesRecursive" ~: (do
          e <- listFilesRecursive [FilePathEx "./tv/00/dir2/dir3" ""]
          assertEqual "00_10 empty dir." [] e)
          
        ,"00_11_listFilesRecursive" ~: (do
          e <- listFilesRecursive [FilePathEx "./tv/00/dir2/" ""]
          assertEqual "00_11 only single dir." [] e)
          
        ,"00_12_listFilesRecursive" ~: (do
          e <- listFilesRecursive [FilePathEx "./tv/00/dir1" "", FilePathEx "./tv/00/dir2/" ""]
          assertEqual "00_12 multi delectories search." [FilePathEx "./tv/00/dir1" "/file3", FilePathEx "./tv/00/dir1" "/file4"] e)

        ,"00_13_listFilesRecursive" ~: (do
          e <- listFilesRecursive [FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" ""]
          assertEqual "00_13 search music folder"
           [FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/01-Dead Or Alive.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/02-恋色折紙.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/03-Whistle Of Dragonfly.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/04-Catastrophe.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/05-リトル妹ドロップス.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/06-Country Road.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/07-Table Mountain.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/08-ひつじの樹海.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/09-Ground Zero.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/10-デート・ア・ライブ 幻のジングル・メドレー.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/11-デート・イン・ユートピア (Game Size).flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/12-Main Theme ～ Supernomal Existence ～ A Looming Crisis.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/13-Dream ～ Quiet Day ～ Morning.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/14-Comical ～ Friends ～ Trio.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/15-Festival ～ Let's Date! ～ Delightful Time.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/16-Amusement ～ Occult ～ Doubt.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/17-Theme Of Fraxinus ～ Evening ～ Pessimism ～ Small Happiness.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/18-Report ～ Nostalgia ～ Threat.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/19-The Perverted World ～ Decisive Battle ～ Utopia ～ Left Behind.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/DATE A LIVE/Date A Music Extension/20-きっと ずっと (Game Size).flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Evolution SOUNDTRACK - Special Edition - I/01-クロスベルの午後.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Evolution SOUNDTRACK - Special Edition - I/02-降水確率10_.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Evolution SOUNDTRACK - Special Edition - I/03-アルモリカ村.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Evolution SOUNDTRACK - Special Edition - I/04-鉱山町マインツ.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Evolution SOUNDTRACK - Special Edition - I/05-聖ウルスラ医科大学.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Evolution SOUNDTRACK - Special Edition - I/06-リベールからの風.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Evolution SOUNDTRACK - Special Edition - I/07-way of life - ZERO NO KISEKI Evolution opening version -.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Original Soundtrack mini/01-way of life -Opening Version-.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Original Soundtrack mini/02-叡智への誘い.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Original Soundtrack mini/03-On The Green Road.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Original Soundtrack mini/04-Inevitable Struggle.flac",FilePathEx "/mnt/d/メディア/Music/CD/アニメ・ゲーム" "/Falcom Sound Team J.D.K/英雄伝説 零の軌跡 Original Soundtrack mini/05-Get Over The Barrier! -Roaring Version-.flac"]
           e)

        ,"01_00_getFlacMetadataFromFile" ~: (do
          e <- getFlacMetadataFromFile "./tv/01/tone1.flac"
          assertEqual "01_00 flac matadata retrival." [MetaStr "MAGIC" "fLaC",MetaStr "vender_string" "Lavf56.40.101",MetaInt "user_comment_list_length" 8,MetaStr "date" "",MetaStr "artist" "",MetaStr "title" "",MetaStr "genre" "",MetaStr "DESCRIPTION" "",MetaStr "IENG" "",MetaStr "copyright" "",MetaStr "encoder" "Lavf56.40.101"]  e)


        ,"01_01_getFlacMetadataFromFile" ~: (do
          e <- getFlacMetadataFromFile "./tv/01/tone2.flac"
          assertEqual "01_01 flac matadata retrival." [MetaStr "MAGIC" "fLaC",MetaStr "vender_string" "Lavf56.40.101",MetaInt "user_comment_list_length" 19,MetaStr "DATE" "2021-01-01",MetaStr "ARTIST" "sample artist",MetaStr "TITLE" "sample title",MetaStr "GENRE" "sample genre",MetaStr "DESCRIPTION" "",MetaStr "IENG" "",MetaStr "copyright" "",MetaStr "encoder" "Lavf56.40.101",MetaStr "DISCTOTAL" "2",MetaStr "TOTALTRACKS" "10",MetaStr "DISCNUMBER" "1",MetaStr "TITLESORT" "ｓａｍｐｌｅ　ｔｉｔｌｅ",MetaStr "ALBUMSORT" "ｓａｍｐｌｅ　ａｌｂｕｍ",MetaStr "COMPOSER" "sample creator",MetaStr "ALBUM" "sample album",MetaStr "TRACKNUMBER" "2",MetaStr "ALBUMARTIST" "sample album artist",MetaStr "ALBUMARTISTSORT" "ｓａｍｐｌｅ　ａｌｂｕｍ　ａｒｔｉｓｔ",MetaStr "ARTISTSORT" "ｓａｍｐｌｅ　ａｒｔｉｓｔ"]  e)
      ]
