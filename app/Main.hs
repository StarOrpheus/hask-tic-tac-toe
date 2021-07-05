module Main ( main ) where

import System.Environment (getArgs, getProgName)

import Client (clientMain)
import Server (serverMain)

parseArgs :: [String] -> IO ()
parseArgs ["server", port] = serverMain (read port)
parseArgs ["client", ip, port] = clientMain ip (read port)
parseArgs _ = do
    exeName <- getProgName
    putStrLn "Usage: "
    putStrLn $ exeName ++ " server port"
    putStrLn $ exeName ++ " client hostIp port"

main :: IO ()
main = getArgs >>= parseArgs
