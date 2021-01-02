module Main where

import Andromeda.Hardware.HDL
import Andromeda.Hardware.JsonInterpreter
import Andromeda.Hardware.Types

exampleSensorDef :: ComponentDef
exampleSensorDef = ComponentDef Sensors "someguid1" "some manufacturer" "some comp name"

exampleControllerDef :: ComponentDef
exampleControllerDef = ComponentDef Controllers "someguid2" "some manufacturer2" "some comp name2"

mkHdl :: Hdl ()
mkHdl = do
  sensor exampleSensorDef "someIndex1" Temperature
  sensor exampleSensorDef "someIndex2" Pressure
  controller exampleControllerDef "someIndex3" 

main :: IO ()
main = do
  let s = toJson mkHdl
  putStr s
  
