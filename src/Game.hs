module Game ( makeMove
            ) where

import GameState

import Data.Vector (findIndex, (//))

makeMove :: GameState -> GameState
makeMove self@(GState board@(GBoard field _) player) = do
    let maybeInd = findIndex cellEmpty field
    let construct field = GState board { boardContent = field } (otherPlayer player)
    case maybeInd of
        Nothing  -> self
        Just pos -> construct $ field // [(pos, GCell $ Just player)]
