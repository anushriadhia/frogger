import Graphics.Vty
import Control.Concurrent
import Control.Exception
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
    failure <- (try $ loop vty input tvarGS) :: IO (Either IOException ()) 
    case failure of 
      Left e -> print e
      Right _ -> print "complete"
    shutdown vty
    shutdownInput input

readInput :: TVar GameState  -> Vty -> IO ()
readInput tvarGS vty  = forever $ do 
  event <- nextEvent vty
  atomically $ do
    gs <- readTVar tvarGS
    writeTVar tvarGS $  gs { previousPlay = Just event }

loop :: Vty -> Input -> TVar GameState -> IO ()
loop vty input tvarGS =  do
    threadDelay 16000
    nextState <- atomically $ do
      s <- readTVar tvarGS
      let f = framesUntilNextCarIsSpawned s - 1
      let nextState = if f == 0 then spawnNewCar s
                                else s { framesUntilNextCarIsSpawned = f }

      writeTVar tvarGS nextState
      return nextState
 
    let pic = gameToImage nextState
    update vty pic -- draw picture on screen
    when (previousPlay nextState /= Just (EvKey KEsc [])) (loop vty input tvarGS)

spawnNewCar :: GameState -> GameState
spawnNewCar gs = gs { cars = (1,0):cars gs, framesUntilNextCarIsSpawned = 1000 }
 
gameToImage :: GameState -> Picture
gameToImage g = picForLayers [player, vertCat roads <-> finishLine <-> prevPlay <-> info] where
    player = string currentAttr "x"
    roads = [1..numRoads g] >>= \i -> [ charFill (defAttr `withBackColor` green) ' ' 40 1
                                      , charFill (defAttr `withBackColor` blue) ' ' 40 1
                                      , charFill (defAttr `withBackColor` blue) ' ' 40 1
                                      ]
    finishLine = charFill (defAttr `withBackColor` white) ' ' 40 1
    prevPlay = string currentAttr (show (previousPlay g))
    info = string currentAttr ("frames: " ++ show (framesUntilNextCarIsSpawned g))
      <->
        string currentAttr ("cars: " ++ show (cars g))

initialGame = GameState
    { playerPosition = (0, 0)
    , framesUntilNextCarIsSpawned = 50
    , numRoads = 2
    , cars = []
    , previousPlay = Nothing
    }

data GameState = GameState
    { playerPosition :: Position
    , framesUntilNextCarIsSpawned :: Int
    , previousPlay :: Maybe Event
    , numRoads :: Integer
    , cars :: [Position]
    }

type Position = (Int, Int) 


