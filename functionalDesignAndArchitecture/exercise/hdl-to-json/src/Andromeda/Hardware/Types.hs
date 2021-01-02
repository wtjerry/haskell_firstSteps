module Andromeda.Hardware.Types where

type Guid = String
type ComponentIndex = String

data Parameter = Temperature | Pressure deriving (Show)
data ComponentClass = Sensors | Controllers deriving (Show)

temperature :: Parameter
temperature = Temperature

pressure :: Parameter
pressure = Pressure

data ComponentDef = ComponentDef
  { componentClass :: ComponentClass
  , componentGuid :: Guid
  , componentManufacturer :: String
  , componentName :: String
  }

