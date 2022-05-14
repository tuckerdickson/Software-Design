import Grammar
import Tokens
import Pir
import System.Environment

{--------------------- My attempted interpreter ---------------------}
-- type Identifier = String
-- type Environment = [(Identifier, Value)]

-- data Value = TruthVal String

-- data Reference 
--   = Variable String
--   | Projection Reference Int

-- data Expression
--   = FunCall String Reference Reference


-- eval :: Expression -> Environment -> Value


-- get env id = snd $ head $ filter (\(id', _) -> id == id') env
-- put env id ex = (id, eval ex env): env
{-------------------------------------------------------------------}

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  writeFile "parsed.pir" (show (parsePir (alexScanTokens s)))
