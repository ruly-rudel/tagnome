import Test.HUnit
import Control.Monad ( void )
import TAGnome.Files

main :: IO ()
main =
    void $ runTestTT $ TestList
      [
         "00_00_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/notexist"
          assertEqual "00_00 dir not exists." [] e)

        ,"00_01_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/file1"
          assertEqual "00_01 not a dir." ["./tv/00/file1"] e)

        ,"00_02_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00"
          assertEqual "00_02 normal 01." ["./tv/00/dir1/file3", "./tv/00/dir1/file4", "./tv/00/file1", "./tv/00/file2"] e)
          
        ,"00_03_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir1"
          assertEqual "00_03 normal 02." ["./tv/00/dir1/file3", "./tv/00/dir1/file4"] e)

        ,"00_04_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir2/dir3"
          assertEqual "00_04 empty dir." [] e)
          
        ,"00_05_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir2"
          assertEqual "00_05 only single dir." [] e)
        {-
         "00_00_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/notexist"
          assertEqual "00_00 dir not exists." e Nothing)

        ,"00_01_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/file1"
          assertEqual "00_01 not a dir." e $ Just ["./tv/00/file1"])

        ,"00_02_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/"
          assertEqual "00_02 normal 01." e (Just ["./tv/00/dir1", "./tv/00/dir2", "./tv/00/file1", "./tv/00/file2"]))
          
        ,"00_03_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir1/"
          assertEqual "00_03 normal 02." e (Just ["./tv/00/dir1/file3", "./tv/00/dir1/file4"]))

        ,"00_04_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir2/dir3"
          assertEqual "00_04 empty dir." e (Just ["./tv/00/dir2/dir3"]))
          
        ,"00_05_listFilesRecursive" ~: (do
          e <- listFilesRecursive1 "./tv/00/dir2/"
          assertEqual "00_05 only single dir." e (Just ["./tv/00/dir2/dir3"]))
        -}
          
        {-
        TestLabel "00_00_listFilesRecursive" test00_00,
        TestLabel "00_01_listFilesRecursive" test00_01,
        TestLabel "00_02_listFilesRecursive" (TestCase (do
          e <- listFilesRecursive "./tv/00"
          assertEqual "00_02 normal 01." e (Just ["file2", "file1", "dir2", "dir1"])))
        -}
      ]
    