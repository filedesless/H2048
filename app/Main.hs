{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Brick (Widget, str)
import Brick.AttrMap (attrMap)
import Brick.Main
import Brick.Types (BrickEvent (..), EventM)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import Control.Monad.State.Class
import Graphics.Vty
import H2048
import System.Random

draw :: State -> [Widget ()]
draw (grid, _) = [center $ str $ showGrid grid]

type State = (Grid, StdGen)

updateState :: (MonadState State m) => (Grid -> Grid) -> m ()
updateState f = do
  (grid, stdGen) <- get
  let (randomLine, stdGen') = randomR (0, 3) stdGen
  let newGrid = addTwoToLine randomLine $ f grid
  put $ (newGrid, stdGen')
  return ()

handleEvent :: BrickEvent n e -> EventM n State ()
handleEvent (VtyEvent (EvKey KUp _)) = updateState (move DirUp)
handleEvent (VtyEvent (EvKey KDown _)) = updateState (move DirDown)
handleEvent (VtyEvent (EvKey KLeft _)) = updateState (move DirLeft)
handleEvent (VtyEvent (EvKey KRight _)) = updateState (move DirRight)
handleEvent (VtyEvent (EvResize _ _)) = return ()
handleEvent _ = halt

app :: App State e ()
app =
  App
    { appDraw = draw,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = neverShowCursor
    }

main :: IO ()
main = do
  stdGen <- newStdGen
  void $ defaultMain app (emptyGrid, stdGen)