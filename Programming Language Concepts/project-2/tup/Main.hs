import Grammar
import Tokens
import Tup
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  writeFile "parsed.tup" (show (parseTup (alexScanTokens s)))
  --putStrLn $ show $ parseTup $ alexScanTokens s
