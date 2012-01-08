import Wbxml.Parser
import Wbxml.SimpleRender
import qualified Data.ByteString as B
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO

data Settings = Settings {
      input :: IO B.ByteString
    , output :: String -> IO()
    }

data Flag = Usage | Input String | Output String 

options :: [OptDescr (Settings -> IO Settings)]
options = [ Option ['i'] ["input"] (ReqArg inp "FILE") "input file name"
          , Option ['o'] ["output"] (ReqArg out "FILE") "output file name"
          ]

inp a o = return o { input = B.readFile a }
out a o = return o { output = writeFile a }

defaultSettings = Settings B.getContents putStrLn

main = do
    args <- getArgs
    settings <- case getOpt RequireOrder options args of
        (f, [], [])     -> foldl (>>=) (return defaultSettings) f 
        (_, _, msgs)    -> error $ concat msgs ++ usage
    process settings
        where usage = usageInfo "Usage: wbxmlxml [OPTIONS]:" options

process s = do
    d <- input s
    case parseWbxml d of
        Left e    -> error e
        Right doc -> case renderWbxml doc of
                        Left e -> error e
                        Right r -> output s r

