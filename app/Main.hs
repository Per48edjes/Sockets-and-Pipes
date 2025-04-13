module Main (main) where

import Book

main :: IO ()
main = do
  writeGreetingFile
  putStrLn "Greeting file written."
  putStrLn "You can check the file at ~/.local/share/sockets-and-pipes/greeting.txt"
  printFileContentsUpperCase

-- howManyHandles
