import Prelude hiding (length, filter)

import ApiCommons
import Server
import GameState

import Control.Concurrent
import Test.Hspec
import Network.HTTP.Client
import Servant.Client
import Data.Vector

testBasicConn port = do
    let fieldSize = 4
    forkIO $ serverMain port
    man <- newManager defaultManagerSettings
    let url = BaseUrl Http "localhost" port ""
    let env = mkClientEnv man url
    (Right
        (GState
            (GBoard content size) _
        ) ) <- runClientM (startSessionClient fieldSize) env
    size `shouldBe` fieldSize
    length content `shouldBe` size^2
    let count pred  = length . filter pred
    let moveCnt = count (not . cellEmpty) content
    moveCnt `shouldSatisfy` (<=) 1

main :: IO ()
main = hspec $ do
    describe "Tic-Tac-Toe client-server tests" $ do
        it "Simple session init test" (testBasicConn 25567)