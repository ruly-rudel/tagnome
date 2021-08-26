module Main where

import Tagnome.Files
import Control.Monad (forM_)

main :: IO ()
main =
    {-
    let dir = "src" in do
        list <- listFilesRecursive1 dir
        case list of
            Nothing -> putStrLn (dir ++ " does not exists of is not a directory.")
            Just l -> forM_ l putStrLn
    -}
    return ()


