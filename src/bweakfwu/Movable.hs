
{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
- Copyright (C) 2013 Stian Ellingsen <stian@plaimi.net>
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
-} module Movable where

import Graphics.Gloss.Data.Vector (Vector)

import Time (StepTime)
import Vector ((^+^), (^-^), vecLimitMag)

class Movable a where
  vel          ::  a -> Velocity
  move         ::  a -> StepTime -> a
  targetVel    ::  a -> Velocity
  acceleration ::  a -> Acceleration

updateVelocity ::  Movable a => a -> StepTime -> Velocity
updateVelocity m dt = vel m ^+^ vecLimitMag (dt * acceleration m) dv
  where dv = targetVel m ^-^ vel m

type Acceleration = Float
type Speed = Float
type Velocity = Vector
