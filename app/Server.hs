module Server (main) where

import BasicPrelude
import Web qualified

main :: IO ()
main = do
  putStrLn $ "Starting server on port " <> tshow port
  Web.startServer port
  where
    port = 8080