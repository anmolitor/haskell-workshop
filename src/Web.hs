module Web (startServer) where

import BasicPrelude

-- import Data.Conduit (ConduitT)
-- import Data.Conduit.List qualified as Conduit
-- import GHC.Conc (atomically, newTVarIO, readTVar, writeTVar)
-- import GHC.Generics (Generic)
-- import Network.Wai.Handler.Warp qualified as Warp (run)
-- import Servant
-- import Servant.API.WebSocketConduit
-- import Servant.Server.Generic (AsServer)

-- Product Type for the routes that will be available on our web server.
-- data Routes mode = Routes
--   { _add :: mode :- "add" :> WebSocketConduit Int Int,
--     _helloWorld :: mode :- "hello" :> Get '[JSON] Text
--     -- _hangman :: mode :- "hangman" :> WebSocketConduit Hangman.GameState.Input Hangman.GameState.Output
--   }
--   deriving (Generic)

-- Implementations
-- routes :: Routes AsServer
-- routes =
--   Routes
--     { _add = addStream,
--       _helloWorld = pure "Hello world"
--       -- _hangman = hangman
--     }

-- | A stream that consumes integers and emits the sum of all consumed integers
-- addStream :: MonadIO m => ConduitT Int Int m ()
-- addStream = do
--   state <- liftIO $ newTVarIO 0
--   let handleInput input = do
--         currentState <- readTVar state
--         let newState = currentState + input
--         writeTVar state newState
--         pure newState
--   Conduit.mapM (liftIO . atomically . handleInput)

-- | Convert the routes to an application and add serving the public directory
-- application :: Application
-- application =
--   serve (Proxy @(NamedRoutes Routes :<|> Raw)) $ routes :<|> serveDirectoryWebApp "public"

-- | Start the application on the given port
startServer :: Int -> IO ()
-- startServer port = Warp.run port application
startServer _ = putStrLn "Comment in the dependencies in package.yaml and code above to actually start a server."
