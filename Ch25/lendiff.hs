-- Question 25.1
import           System.Environment
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.IO                  as TIO

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  textContent <- TIO.readFile filename
  let numChars = T.length textContent
  let numBytes = BC.length (E.encodeUtf8 textContent)
  print (numBytes - numChars)
