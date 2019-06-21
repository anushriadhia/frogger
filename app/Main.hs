import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Control.Monad.STM
import GHC.Conc
import Graphics.Vty

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

readInput :: TVar GameState -> Vty -> IO ()
readInput tvarGS vty =
  forever $ do
    event <- nextEvent vty
    atomically $ do
      gs <- readTVar tvarGS
      writeTVar tvarGS $ getGameState gs event

getGameState :: GameState -> Event -> GameState
getGameState gs (EvKey KRight []) =
  let (x, y) = playerPosition gs
   in gs {playerPosition = (x + 1, y)}
getGameState gs (EvKey KLeft []) =
  let (x, y) = playerPosition gs
   in gs {playerPosition = (x - 1, y)}
getGameState gs (EvKey KUp []) =
  let (x, y) = playerPosition gs
   in gs {playerPosition = (x, y - 1)}
getGameState gs (EvKey KDown []) =
  let (x, y) = playerPosition gs
   in gs {playerPosition = (x, y + 1)}
getGameState gs evt = gs {previousPlay = Just evt}

loop :: Vty -> Input -> TVar GameState -> IO ()
loop vty input tvarGS = do
  threadDelay 16000
  nextState <-
    atomically $ do
      s <- advance `fmap` readTVar tvarGS
      writeTVar tvarGS s
      return s
  let pic = gameToImage nextState
  update vty pic -- draw picture on screen
  when (previousPlay nextState /= Just (EvKey KEsc [])) (loop vty input tvarGS)

computeCarPositions :: GameState -> [Position]
computeCarPositions gs =
  let numCars = width gs `div` 10
   in [ (((frames gs `div` 10 * direction) + carNum * 10) `mod` width gs, y)
      | roadNum <- [0 .. numRoads gs - 1]
      , carNum <- [0 .. numCars]
      , (direction, y) <-
          [ (1, roadNum * 3 + 1) -- east-bound
          , (-1, roadNum * 3 + 2) -- west-bound
          ]
      ]

advance :: GameState -> GameState
advance s = s {frames = frames s + 1}

gameToImage :: GameState -> Picture
gameToImage g =
  picForLayers $
  car2img `map` computeCarPositions g ++
  [player (playerPosition g), vertCat roads <-> finishLine <-> prevPlay <-> info]
  where
    player (x, y) = translate x y $ string currentAttr "x"
    car2img (x, y) = translate x y $ string currentAttr "X"
    roads =
      [1 .. numRoads g] >>
      [ charFill (defAttr `withBackColor` green) ' ' (width g) 1
      , charFill (defAttr `withBackColor` blue) ' ' (width g) 1
      , charFill (defAttr `withBackColor` blue) ' ' (width g) 1
      ]
    finishLine = charFill (defAttr `withBackColor` white) ' ' (width g) 1
    prevPlay = string currentAttr (show (previousPlay g))
    info = string currentAttr ("frames: " ++ show (frames g))

initialGame =
  GameState {playerPosition = (0, 0), numRoads = 2, previousPlay = Nothing, width = 40, frames = 0}

data GameState =
  GameState
    { playerPosition :: Position
    , previousPlay :: Maybe Event
    , numRoads :: Int
    , width :: Int
    , frames :: Int
    }

type Position = (Int, Int)
