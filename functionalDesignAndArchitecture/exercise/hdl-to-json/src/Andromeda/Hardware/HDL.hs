module Andromeda.Hardware.HDL where

import Control.Monad.Free
import Andromeda.Hardware.Types

data Component a
  = SensorDef ComponentDef ComponentIndex Parameter a
  | ControllerDef ComponentDef ComponentIndex a

instance Functor Component where
  fmap f (SensorDef cd ci p a) = SensorDef cd ci p (f a)
  fmap f (ControllerDef cd ci a) = ControllerDef cd ci (f a)

type Hdl a = Free Component a

sensor :: ComponentDef
       -> ComponentIndex
       -> Parameter
       -> Hdl ()
sensor cd ci p = Free (SensorDef cd ci p (Pure ()))

controller :: ComponentDef
           -> ComponentIndex
           -> Hdl ()
controller cd ci = Free (ControllerDef cd ci (Pure ()))
