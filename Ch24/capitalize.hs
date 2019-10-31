import           System.Environment
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  input <- TIO.readFile filename
  TIO.writeFile filename (T.toUpper input)
