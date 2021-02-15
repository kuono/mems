module MyLibTest (main, dummyData807d) where

import Mems
import System.Random ( newStdGen, Random(randoms) )
import qualified Data.ByteString.Char8 as BS
import Data.Char ( ord, chr )

main :: IO ()
main = putStrLn "Test suite not yet implemented."

dummyData807d :: IO RData
dummyData807d = do
  g <- newStdGen
  let r = randoms g :: [Char]
      r1 = Prelude.take 27 r 
      r2 = Prelude.take 31 $ Prelude.drop 28 r 
  return (BS.pack (chr 28:r1),BS.pack (chr 32:r2))-- random807d :: IO ECU.Data807d

-- emptyFrame :: Frame
-- emptyFrame = Frame 
--   { d80size     = 28, d7dsize = 32, engineSpeed = 0 , coolantTemp = 0 , ambientTemp = 0 , intakeATemp = 0
--   , fuelTemp    = 0 , mapSensor   = 0 , battVoltage = 0.0 , ibattVoltage = 0
--   , throttlePot = 0.0 , ithrottlePot = 0 , idleSwitch  = False , idleByte = 0
--   , unknown0B   = 0 , pnClosed    = 0
--   , faultCode1  = False , faultCode2  = False , faultCodeX4 = False , faultCodeX5 = False
--   , faultCode10 = False , faultCodeY5 = False , faultCode16 = False
--   , faultCode0D = 0 , faultCode0E = 0
--   , unknown0F   = 0 , unknown10   = 0
--   , unknown11   = 0 , idleACMP    = 0 , idleSpdDev  = 0 , unknown15   = 0 , ignitionAd  = 0.0
--   , coilTime    = 0.0 , unknown19   = 0 , unknown1A   = 0 , unknown1B   = 0
--   , lambda_voltage = 0 , closed_loop'   = 0 , fuel_trim'     = 0
--   }
--
-- dummyFrameData :: IO Frame
-- dummyFrameData = parse <$> dummyData807d

