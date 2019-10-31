import           System.Environment
import qualified Data.Text.IO                  as TIO

main :: IO ()
main = do
  args <- getArgs
  let sourceFilename      = head args
  let destinationFilename = args !! 1
  source <- TIO.readFile sourceFilename
  TIO.writeFile destinationFilename source
