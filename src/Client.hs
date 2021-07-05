{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Client ( clientMain
              ) where

import ApiCommons
import GameState

import Network.HTTP.Client
import Servant.Client

import Control.Concurrent
import Control.Lens
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Data.Array

data GameEvent = SetState GameState
               | GameFinished String
               deriving (Eq, Show)

data GameAction = StartSession  Int
                | MakeMove      GameState

--- Channel, that gonna send events FROM ioThread
type IOChan = BChan GameEvent

--- Channel, that gonna send actions TO ioThread
type ActionChan = BChan GameAction

ioThread :: String
         -> Int
         -> IOChan
         -> ActionChan
         -> IO ()
ioThread host port outChan inChan = do
    man <- newManager defaultManagerSettings
    let url = BaseUrl Http host port ""
    let env = mkClientEnv man url
    forever $ do
        act <- readBChan inChan
        result <- case act of
            StartSession fieldSize -> runClientM (startSessionClient fieldSize) env
            MakeMove newState      -> runClientM (makeMoveClient newState) env
        case result of
            Left err -> writeBChan outChan $ GameFinished $ show err
            Right s  -> writeBChan outChan $ SetState s

data GameSceneState = GameSceneState
    { _gState        :: GameState        --- current GameState
    , _gSellectedPos :: CellPos          --- selected position on the board
    , _gPlayerMark   :: GPlayerMark      --- player mark (x/o)
    }

makeLenses ''GameSceneState

newtype MenuSceneState = MenuSceneState
    { _mSellectedFieldSize  :: Int
    }

makeLenses ''MenuSceneState

data Scene = InGame GameSceneState
           | InMenu MenuSceneState
makePrisms ''Scene

data ProgState = ProgState
    { _scene :: Scene
    , _aChan :: ActionChan
    }

makeLenses ''ProgState

filledCellAttr, emptyCellAttr, emptyAttr :: AttrName
selCellAttr :: AttrName
filledCellAttr = "selCellAttr"
emptyCellAttr = "emptyCellAttr"
selCellAttr = "selCellAttr"
emptyAttr = "emptyAttr"

--- Simple color palette
colorPalette :: Array Int V.Color
colorPalette = array (0, 3)
    [ (0, V.rgbColor 3 40 40)
    , (1, V.rgbColor 24 55 135)
    , (2, V.rgbColor 179 100 224)
    , (3, V.rgbColor 255 214 240)]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (selCellAttr, (colorPalette!0) `on` (colorPalette!3))  --- doesn't work smh..
  , (filledCellAttr, (colorPalette!0) `on` (colorPalette!2))
  , (emptyCellAttr, (colorPalette!2) `on` (colorPalette!0))
  ]

app :: App ProgState GameEvent ()
app = App
    { appDraw = drawTUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

drawTUI :: ProgState -> [Widget ()]
drawTUI s =
    case s^.scene of
        InMenu s -> [ C.center $ drawMenu s ]
        InGame s -> [ C.center $ drawGame s <=> str ("Your mark is " ++ show (s ^. gPlayerMark)) ]

drawMenu :: MenuSceneState -> Widget ()
drawMenu scene = vBox $ do
    let decorate sz =
            if sz == scene^.mSellectedFieldSize then
                "> " <> show sz <> " <"
            else
                show sz
    map (C.hCenter . str . decorate) [3..5]

drawGame :: GameSceneState -> Widget ()
drawGame scene =
    case getStatus (gameBoard $ scene^.gState) of
        InProgress -> drawGameBoard scene
        s -> str $
            "Game Over - " ++ show s

drawGameBoard :: GameSceneState -> Widget ()
drawGameBoard scene = do
    let board = gameBoard $ scene^.gState
    let fieldSz = boardSize board
    let selected = scene ^. gSellectedPos

    let drawCell x y board = do
            let (GCell maybeMk) = cellAt x y board
            let isSel = selected == cellPos x y fieldSz
            let fAttr = withAttr $ if isSel then selCellAttr else filledCellAttr
            let eAttr = withAttr $ if isSel then selCellAttr else emptyCellAttr
            case maybeMk of
                (Just mk) -> fAttr $ str $ show mk
                Nothing   -> eAttr $ str "."
    let cellsInRow x = [drawCell x y board | y <- [0..fieldSz-1]]
    let rows = [hBox $ cellsInRow r | r <- [fieldSz-1, fieldSz-2..0]]
    withBorderStyle BS.unicodeBold $
        B.border $
            vBox rows

moveSelHandler :: Int
               -> Int
               -> ProgState
               -> ProgState
moveSelHandler dx dy state = state
                           & scene._InGame.gSellectedPos %~ advance dx dy
                           & scene._InMenu.mSellectedFieldSize %~ moveBoardSizeSelector dx
    where
        moveBoardSizeSelector :: Int        --- d(size)
                              -> Int        --- current size
                              -> Int        --- advance result
        moveBoardSizeSelector d sz = do
            let newSz = sz - d
            if newSz >= 3 && newSz <= 5 then
                newSz
            else
                sz

handleEvent :: ProgState -> BrickEvent () GameEvent -> EventM () (Next ProgState)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (AppEvent (GameFinished _))           = halt s
handleEvent s (AppEvent (SetState state))           = do
    let sz = boardSize (gameBoard state)
    let startPos = CellPos 0 sz
    let playerMk = currentPlayer state
    let newProgState = s & scene .~ InGame (GameSceneState state startPos playerMk)
    continue newProgState

handleEvent s (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveSelHandler 0 (-1) s
handleEvent s (VtyEvent (V.EvKey V.KRight []))      = continue $ moveSelHandler 0 1 s
handleEvent s (VtyEvent (V.EvKey V.KUp []))         = continue $ moveSelHandler 1 0 s
handleEvent s (VtyEvent (V.EvKey V.KDown []))       = continue $ moveSelHandler (-1) 0 s
handleEvent s (VtyEvent (V.EvKey V.KEnter []))      =
    case s^.scene of
        (InMenu (MenuSceneState sellSz)) -> do
            liftIO $ writeBChan (s^.aChan) (StartSession sellSz)
            continue s
        (InGame scene'@(GameSceneState gState' selPos player)) ->
            if player /= currentPlayer gState'
            || not (cellEmpty (getCell selPos (gameBoard gState') )) then
                continue s
            else do
                let newScene = onPlayerMarkCell scene'
                liftIO $ writeBChan (s^.aChan) (MakeMove $ newScene^.gState)
                continue $ s & scene._InGame.~newScene

handleEvent s _ = continue s

onPlayerMarkCell :: GameSceneState
                 -> GameSceneState
onPlayerMarkCell scene = do
    let state = scene^.gState
    let pos = scene^.gSellectedPos
    let mark = scene^.gPlayerMark
    let board = gameBoard state
    let newBoard = setCell pos (GCell $ Just mark) board
    let newState = state { gameBoard=newBoard, currentPlayer=otherPlayer mark }
    scene & gState .~ newState


clientMain :: String -> Int -> IO ()
clientMain host port = do
    ioChan <- newBChan 8
    actionChan <- newBChan 8
    forkIO $ ioThread host port ioChan actionChan
    let state = ProgState (InMenu $ MenuSceneState 3) actionChan
    -- let state = ProgState (InGame $ GameSceneState (newGameState 5) (CellPos 0 5) GPlayerX) actionChan

    let mkVty = V.mkVty V.defaultConfig
    initialVty <- mkVty
    void $ customMain initialVty mkVty (Just ioChan) app state
