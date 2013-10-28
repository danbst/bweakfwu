{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
-
- This file is part of bweakfwu
-
- bweakfwu is free software: you can redistribute it and/or modify
- it under the terms of the GNU General Public License as published by
- the Free Software Foundation, either version 3 of the License, or
- (at your option) any later version.
-
- bweakfwu is distributed in the hope that it will be useful,
- but WITHOUT ANY WARRANTY; without even the implied warranty of
- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
- GNU General Public License for more details.
-
- You should have received a copy of the GNU General Public License
- along with bwekfwu  If not, see <http://www.gnu.org/licenses/>.
import Graphics.Gloss.Data.Color
-}

{-# LANGUAGE Arrows #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Display
import qualified Control.Wire as Wire
import Control.Wire ((<|>), (.))
import Prelude hiding ((.), id)
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow
import System.IO.Unsafe

import Visible
import Visible.Board
import Visible.ScoreKeeper
import Movable.Paddle
import Movable.Ball
import World (worldHeight, worldWidth)

--import Handle (handle)
--import Window (window)
--import World (bang, step, view)

windowWidth ::  Int
windowWidth = 640

windowHeight ::  Int
windowHeight = 480

main ::  IO ()
main =
  play
  (InWindow "bweakfwu netwired" (windowWidth, windowHeight) (10, 10))
  white  -- The background colour
  600    -- The number of simulation steps per second
  bang   -- Create initial world
  view   -- Convert the world to a picture
  handle -- Handle input
  step   -- Step the world one iteration

data Player = Player {
    _paddle :: Paddle
  , _ball :: Ball
  , _score :: Score
}
data World' = World {
    _leftPlayer :: Player
  , _rightPlayer :: Player
  , _board :: Board
}

type World = Maybe World'

type Input = Set.Set Key

data W = W {
    _wire :: Wire.Wire () Identity Input World'
  , _world :: World
  , _input :: Input
}

mainWire :: Wire.Wire () Identity Input World'
mainWire = proc input -> do
    rec
      (p1y_input, p2y_input) <- (Wire.pure (0.1, 0) . Wire.when (Set.member $ Char 'w') 
                            <|> Wire.pure (-0.1, 0) . Wire.when (Set.member $ Char 's'))
                            <|> (Wire.pure (0, 0.1) . Wire.when (Set.member $ Char 'i')
                            <|> Wire.pure (0, -0.1) . Wire.when (Set.member $ Char 'k'))
                            -< input
      p1y <- Wire.accum (+) 0 -< p1y_input
      p2y <- Wire.accum (+) 0 -< p2y_input
      
    --p1y <- Wire.accum (+) 0 . (0.1 . Wire.when (charPressed 'w') <|> (-0.1) . Wire.when (charPressed 's')) -< input
    returnA -< World { _board = Board $ brickBoard 10 10
                     , _leftPlayer  = Player { _paddle = simplePaddle (-35.0) p1y yellow
                                             , _ball = simpleBall, _score = 0 }
                     , _rightPlayer = Player { _paddle = simplePaddle 35.0 p2y black
                                             , _ball = simpleBall, _score = 0 }
                     }
  where simpleBall = Ball (-34.0, 0) 0.5 yellow basicVelocity
        specialPressed k = Set.member (SpecialKey k)
        charPressed k = Set.member (Char k)
        simplePaddle x y color = Paddle (x, y) (1, 5) color basicVelocity (False, False, False)
        keyInput k1 k2 = 0.1 . Wire.when (Set.member k1) <|> (-0.1) . Wire.when (Set.member k2)
        {-processPaddleKeys = mkPure $ \_ input = 
            MkPure (Right (p1y, p2y), processPaddleKeys)-}

--Wire.when (Set.member (SpecialKey KeyLeft)) >>> Wire.pure 0.1 >>> Wire.accum (+) 0 >>> movePaddle1
    --where movePaddle1 = Wire.mkPure $ \dt a -> (Right $ World { _circle = (a, 0) }, moveCircle)

bang :: W
bang = W { _wire = mainWire, _world = Nothing, _input = Set.empty }

view :: W -> Picture
view w = case _world w of
   Nothing -> blank
   Just w  -> viewWorld w

viewWorld ::  World' -> Picture
viewWorld (World { _leftPlayer = Player { _paddle = p1
                                        , _ball = b1
                                        , _score = score1
                                        }
                 , _rightPlayer = Player { _paddle = p2
                                         , _ball = b2
                                         , _score = score2
                                         }
                 , _board = board 
                 }) = 
  Scale worldScale worldScale
  $ Pictures [render p1
             ,render p2
             ,render b1
             ,render b2
             ,render board
             ,render (ScoreKeeper score1 score2)
             ,Color white $ rectangleWire 80 45]
  where wh         = fromIntegral windowHeight
        ww         = fromIntegral windowWidth
        worldScale = min (wh / worldHeight) (ww / worldWidth)

handle :: (Event -> W -> W)
handle e w = case e of
    EventKey k Down _ _ -> w { _input = Set.insert k (_input w) }
    EventKey k Up   _ _ -> w { _input = Set.delete k (_input w) }
    _                   -> w
    
step :: (Float -> W -> W)
step time w =
    let (mx, w') = Wire.stepWireP (_wire w) (fromRational $ toRational time) (_input w)
    in case mx of
        Left ex -> w { _wire = w' }
        Right x -> w { _wire = w', _world = Just x }
        
debug :: (Show a) => Wire.Wire () Identity a a
debug = Wire.mkPure $ \dt x -> (Right $ (unsafePerformIO $ putStrLn $ show x) `seq` x, debug)
