{-# LANGUAGE DeriveGeneric #-}

module GameState
    ( GPlayerMark (..)
    , GCell (..)
    , GBoard (..)
    , GameState (..)
    , CellPos (..)
    , cellPos
    , cellAt
    , getCell
    , setCell
    , advance
    , newGameState
    , cellEmpty
    , otherPlayer
    , getStatus
    , GameStatus (..)
    ) where

import Control.Monad
import Data.Aeson
import qualified Data.List as L
import Data.Maybe
import qualified Data.Vector as Vec
import GHC.Generics

data GameStatus = InProgress
                | Tie
                | PlayerWin GPlayerMark
                deriving (Eq)

instance Show GameStatus where
    show InProgress     = "In progress"
    show Tie            = "Tie"
    show (PlayerWin pl) = "Player " ++ show pl ++ " won!"

data GPlayerMark = GPlayerX
                 | GPlayerO
                 deriving (Eq, Generic)

instance Show GPlayerMark where
    show GPlayerO = "O"
    show GPlayerX = "X"

newtype GCell = GCell (Maybe GPlayerMark)
              deriving (Eq, Show, Generic)

data GBoard = GBoard
    { boardContent :: Vec.Vector GCell
    , boardSize    :: Int
    } deriving (Eq, Show, Generic)

data GameState = GState
    { gameBoard     :: GBoard
    , currentPlayer :: GPlayerMark
    } deriving (Eq, Show, Generic)

instance ToJSON GPlayerMark
instance FromJSON GPlayerMark

instance ToJSON GCell
instance FromJSON GCell

instance ToJSON GBoard
instance FromJSON GBoard

instance ToJSON GameState
instance FromJSON GameState

data CellPos = CellPos Int Int

instance Show CellPos where
    -- show :: CellPos -> String
    show (CellPos pos sz) = do
        let xPos = pos `div` sz
        let yPos = pos `mod` sz
        "CellPos(" ++ show xPos ++ "; " ++ show yPos ++ "; pos=" ++ show pos ++ "; sz=" ++ show sz ++ ")"

instance Eq CellPos where
    (==) (CellPos pos1 sz1) (CellPos pos2 sz2) =
        if sz1 /= sz2 then
            error "Comparing of CellPos of differrent size"
        else
            pos1 == pos2

cellPos :: Int  --- X coord
        -> Int  --- Y coord
        -> Int  --- Field size [3..5]
        -> CellPos
cellPos x y sz = CellPos (x * sz + y) sz

cellAt :: Int       --- X coord
       -> Int       --- Y coord
       -> GBoard
       -> GCell
cellAt x y (GBoard content sz) = content Vec.! (x * sz + y)

getCell :: CellPos
        -> GBoard
        -> GCell
getCell (CellPos pos sz) (GBoard board bSz) =
    if sz /= bSz then
        error "getCell pos size differs from board size!"
    else
        board Vec.! pos

setCell :: CellPos
        -> GCell
        -> GBoard
        -> GBoard
setCell (CellPos pos _)
        cell
        gBoard@(GBoard board _) =
    gBoard { boardContent=board Vec.// [(pos, cell)] }

advance :: Int      --- dx
        -> Int      --- dy
        -> CellPos  --- pos
        -> CellPos  --- shifted pos
advance dx dy cellPos@(CellPos pos fieldSize) = do
    let xShift = dx * fieldSize
    let yShift = dy
    let newPos = pos + xShift + yShift
    if newPos >= 0 && newPos < fieldSize ^ 2 then
        CellPos newPos fieldSize
    else
        cellPos

newGameState :: Int -> GameState
newGameState fieldSize = do
    let field = Vec.replicate (fieldSize ^ 2) (GCell Nothing)
    let gBoard = GBoard field fieldSize
    GState gBoard GPlayerX

cellEmpty :: GCell -> Bool
cellEmpty (GCell m) = null m

otherPlayer :: GPlayerMark -> GPlayerMark
otherPlayer GPlayerX = GPlayerO
otherPlayer GPlayerO = GPlayerX

checkWinner :: GBoard
            -> Maybe GPlayerMark
checkWinner (GBoard board sz) =
    msum $
        [check [board Vec.! (i * sz + j) | j <- [0..sz-1]] | i <- [0..sz-1]]
     ++ [check [board Vec.! (i * sz + j) | i <- [0..sz-1]] | j <- [0..sz-1]]
     ++ [check [board Vec.! (i * sz + i) | i <- [0..sz-1]]]
     ++ [check [board Vec.! (i * sz + (sz-i-1)) | i <- [0..sz-1]]]
     where
        check :: [GCell] -> Maybe GPlayerMark
        check ((GCell (Just x)):xs) = do
            let win = all (== GCell (Just x)) xs
            if win then Just x else Nothing
        check _ = Nothing

getStatus :: GBoard
          -> GameStatus
getStatus gBoard@(GBoard board _) = do
    let maybeWinner = checkWinner gBoard
    let noSlots = null $ Vec.findIndex cellEmpty board
    case maybeWinner of
        Nothing -> if noSlots then Tie else InProgress
        Just w  -> PlayerWin w
