{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Server ( serverMain ) where

import Servant.API (type (:<|>) (..))

import GameState (GameState, newGameState)

import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import Network.Wai.Handler.Warp (run)
import Servant (Handler, ServerError (errBody), err400, serve)

import System.Random (Random (randomIO))

import Game (makeMove)

import ApiCommons (serverApi)

startSessionHandler :: Int -> Handler GameState
startSessionHandler fieldSize =
    if fieldSize < 3 || fieldSize > 5 then
        throwError $ err400 { errBody = "Bad field size, [3..5] expected" }
    else do
        let state = newGameState fieldSize
        computerFirst <- liftIO randomIO
        return $ if computerFirst then
                    makeMove state
                 else
                    state

makeTurnHandler :: GameState -> Handler GameState
makeTurnHandler state = return $ makeMove state

serverMain :: Int -> IO ()
serverMain port =
    run port $ serve serverApi (startSessionHandler :<|> makeTurnHandler)
