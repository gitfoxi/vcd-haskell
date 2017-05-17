
module JtagStateMachine
  (JtagState(..)
  ,JtagIO(..)
  ,jtagStateNext
  )where

data JtagState = TestLogicReset
               | RunTestIdle
               | SelectDrScan
               | CaptureDr
               | ShiftDr
               | Exit1Dr
               | PauseDr
               | Exit2Dr
               | UpdateDr
               | SelectIrScan
               | CaptureIr
               | ShiftIr
               | Exit1Ir
               | PauseIr
               | Exit2Ir
               | UpdateIr
               deriving (Eq, Show)


jtagStateNext :: JtagState -> Bool -> JtagState
jtagStateNext TestLogicReset True = TestLogicReset
jtagStateNext RunTestIdle    True = SelectDrScan
jtagStateNext SelectDrScan   True = SelectIrScan
jtagStateNext CaptureDr      True = Exit1Dr
jtagStateNext ShiftDr        True = Exit1Dr
jtagStateNext Exit1Dr        True = UpdateDr
jtagStateNext PauseDr        True = Exit2Dr
jtagStateNext Exit2Dr        True = UpdateDr
jtagStateNext UpdateDr       True = SelectDrScan
jtagStateNext SelectIrScan   True = TestLogicReset
jtagStateNext CaptureIr      True = Exit1Ir
jtagStateNext ShiftIr        True = Exit1Ir
jtagStateNext Exit1Ir        True = UpdateIr
jtagStateNext PauseIr        True = Exit2Ir
jtagStateNext Exit2Ir        True = UpdateIr
jtagStateNext UpdateIr       True = SelectDrScan

jtagStateNext TestLogicReset False = RunTestIdle
jtagStateNext RunTestIdle    False = RunTestIdle
jtagStateNext SelectDrScan   False = CaptureDr
jtagStateNext CaptureDr      False = ShiftDr
jtagStateNext ShiftDr        False = ShiftDr
jtagStateNext Exit1Dr        False = PauseDr
jtagStateNext PauseDr        False = PauseDr
jtagStateNext Exit2Dr        False = ShiftDr
jtagStateNext UpdateDr       False = RunTestIdle
jtagStateNext SelectIrScan   False = CaptureIr
jtagStateNext CaptureIr      False = ShiftIr
jtagStateNext ShiftIr        False = ShiftIr
jtagStateNext Exit1Ir        False = PauseIr
jtagStateNext PauseIr        False = PauseIr
jtagStateNext Exit2Ir        False = ShiftIr
jtagStateNext UpdateIr       False = RunTestIdle

data JtagIO =
  JtagIO
  { tms :: Bool
  , tdi :: Maybe Bool
  , tdo :: Maybe Bool
  } deriving Show
