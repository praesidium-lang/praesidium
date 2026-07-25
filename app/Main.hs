module Main where
import System.Environment (getArgs)
import Core.Prlexer (alexScanTokens)

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Welcome to Praesidium's pre-alpha state."
  putStrLn "WARNING: Do not use this compiler for anything important yet, wait until the project is madure."
  case args of
    [filePath] -> do 
      source <- readFile filePath
      putStrLn (show (alexScanTokens source))
    _ -> putStrLn "Usage: ./prc <File Path of .prae file>"
