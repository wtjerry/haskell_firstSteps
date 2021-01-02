module Andromeda.Hardware.Device (
    Device,
    DeviceComponent,
    blankDevice,
    addSensor,
    addController,
--    getComponent,
--    updateComponent,
--    setMeasurement,
--    readMeasurement
) where

import Andromeda.Hardware.Types
import Data.Map as M

type Measurement = ()

data DeviceComponent
  = Sensor Measurement Guid
  | Controller Guid
                     
newtype Device = DeviceImpl (M.Map ComponentIndex DeviceComponent)

blankDevice :: Device
blankDevice = DeviceImpl M.empty

addSensor :: ComponentIndex
          -> Parameter
          -> ComponentDef
          -> Device
          -> Device
addSensor ci _ (ComponentDef _ guid _ _) (DeviceImpl d) = DeviceImpl $ M.insert ci (Sensor () guid) d

addController :: ComponentIndex
          -> ComponentDef
          -> Device
          -> Device
addController ci (ComponentDef _ guid _ _) (DeviceImpl d) = DeviceImpl $ M.insert ci (Controller guid) d
