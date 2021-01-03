module Andromeda.Hardware.JsonInterpreter where

import Control.Monad.Free
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Types

toJson :: Hdl () -> String
toJson = interpretJson ""

interpretJson :: String -> Hdl () -> String
interpretJson s (Pure _) = "[" ++ s ++ "]"
interpretJson s (Free comp) = interpretComponent s comp

interpretComponent :: String -> Component (Hdl ()) -> String
interpretComponent s (SensorDef (ComponentDef cc guid cm cn) ci p next) =
  let s' = s
           ++ "{"
           ++ "\"Type\": \"SensorDef\","
           ++ "\"index\": \"" ++ ci ++ "\","
           ++ "\"parameter\": \"" ++ show p ++ "\","
           ++ "\"class\": \"" ++ show cc ++ "\","
           ++ "\"guid\": \"" ++ guid ++ "\","
           ++ "\"model\": \"" ++ cm ++ "\","
           ++ "\"name\": \"" ++ cn ++ "\""
           ++ "}"
           ++ addTrailingCommaIfRequired next
  in interpretJson s' next
interpretComponent s (ControllerDef (ComponentDef cc guid cm cn) ci next) =
  let s' = s
           ++ "{"
           ++ "\"Type\": \"ControllerDef\","
           ++ "\"index\": \"" ++ ci ++ "\","
           ++ "\"class\": \"" ++ show cc ++ "\","
           ++ "\"guid\": \"" ++ guid ++ "\","
           ++ "\"model\": \"" ++ cm ++ "\","
           ++ "\"name\": \"" ++ cn ++ "\""
           ++ "}"
           ++ addTrailingCommaIfRequired next
  in interpretJson s' next

addTrailingCommaIfRequired :: Hdl () -> String
addTrailingCommaIfRequired (Pure _) = ""
addTrailingCommaIfRequired (Free _) = ","
