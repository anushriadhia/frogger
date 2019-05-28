import Graphics.Vty
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import GHC.Conc

main = do
    cfg <- standardIOConfig

    input <- inputForConfig cfg
    vty <- mkVty cfg

    tvarGS <- newTVarIO initialGame
    forkIO $ readInput tvarGS vty
    -- let line0 = string (defAttr ` withForeColor ` green) "first line"
        -- line1 = string (defAttr ` withBackColor ` blue) "second line"
        -- img = line0 <-> line1
    loop vty input tvarGS
    {-
    failure <- try loop
    case failure of 
      Left e -> print e
      Right _ -> print "complete"
    -}
    shutdown vty
    shutdownInput input

readInput :: TVar GameState  -> Vty -> IO ()
readInput tvarGS vty  = forever $ do 
  event <- nextEvent vty
  atomically $ do
    gs <- readTVar tvarGS
    writeTVar tvarGS $  gs { previousPlay = Just event }
  



loop :: Vty -> Input -> TVar GameState -> IO ()
loop vty input tvarGS = forever $  do
    s <- readTVarIO tvarGS
    let pic = gameToImage s
    update vty pic
    if previousPlay s == Just (EvKey KEsc [])
       then error "escaped"
       else return ()

gameToImage :: GameState -> Picture
gameToImage g = picForLayers [player , vertCat roads <-> finishLine <-> prevPlay] where
    player = string currentAttr "x"
    roads = [1..numRoads g] >>= \i -> [ charFill (defAttr `withBackColor` green) ' ' 40 1
                                      , charFill (defAttr `withBackColor` blue) ' ' 40 1
                                      , charFill (defAttr `withBackColor` blue) ' ' 40 1
                                      ]
    finishLine = charFill (defAttr `withBackColor` white) ' ' 40 1
    prevPlay = string currentAttr (show (previousPlay g))

initialGame = GameState
    { playerPosition = (0, 0)
    , numRoads = 2
    , cars = []
    , previousPlay = Nothing
    }

data GameState = GameState
    { playerPosition :: Position
    , previousPlay :: Maybe Event
    , numRoads :: Integer
    , cars :: [Position]
    }

type Position = (Int, Int) 


