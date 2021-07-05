{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module ApiCommons ( ServerAPI (..)
                  , serverApi
                  , startSessionClient
                  , makeMoveClient
                  ) where

import GameState (GameState)
import Servant (Proxy (..))
import Servant.API (Capture, JSON, Post, ReqBody, type (:<|>) (..), type (:>))
import Servant.Client (client)

type ServerAPI = "startSession" :> Capture "boardSize" Int :> Post '[JSON] GameState
            :<|> "makeTurn" :> ReqBody '[JSON] GameState :> Post '[JSON] GameState

serverApi :: Proxy ServerAPI
serverApi = Proxy

startSessionClient :<|> makeMoveClient = client serverApi
