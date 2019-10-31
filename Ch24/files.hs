import           System.IO

main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  hasLine   <- hIsEOF helloFile
  firstLine <- if not hasLine then hGetLine helloFile else return "empty"
  putStrLn firstLine

  hasSecondLine <- hIsEOF helloFile
  secondLine <- if not hasSecondLine then hGetLine helloFile else return "empty"
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine

  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"
