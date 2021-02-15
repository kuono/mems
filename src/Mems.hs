{- |
* Module      : Mems
* Description : MEMS ( Rover Mini Ecu ) Communication Library
* Copyright   : (c) Kentaro UONO, 2018-2021
* License     : MIT Licence
* Maintainer  : info@kuono.net
* Stability   : experimental
* Portability : macOS X
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Mems 
            ( Event  --    = (BS.ByteString,BS.ByteString)
            , RData  --    = Data807d
            , Frame(..)
            , EvContents(..) -- =  PortNotFound FilePath | Connected ModelDataSet | ...
            , UCommand (..)
            , ModelDataSet(..)
            , Mems.run  -- :: MEMS a -> (FilePath,BC.BChan Event,TChan UCommand,TChan Event) -> IO ()
            , Mems.loop -- :: MEMS ()
            -- type MEMS = ReaderT Env IO
            , parse    -- :: Data807d -> Frame
            , get807d
            , emptyD7d
            , emptyD80
            , emptyData807d
            , dummyData807d
            , mneUnknown
            , mname
) where
import qualified Brick.BChan as BC
import qualified Data.ByteString.Char8 as BS
import qualified Control.Exception as Ex
import Control.Concurrent
    ( threadDelay, forkIO, killThread, ThreadId )
import Control.Concurrent.STM.TChan
    ( readTChan, tryReadTChan, writeTChan, TChan )
import Control.Monad ( forever )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.STM ( atomically )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.Reader ( ask, ReaderT(runReaderT) )
import Data.Bits ( Bits(testBit) )
import Data.Char ( ord, chr )
import Data.Time.LocalTime
    ( utcToLocalTime, getCurrentTimeZone, LocalTime )
import Data.Time.Clock ( getCurrentTime )
import System.Hardware.Serialport
    ( closeSerial,
      flush,
      openSerial,
      recv,
      send,
      defaultSerialSettings,
      SerialPort,
      CommSpeed(CS9600),
      FlowControl(Software),
      SerialPortSettings(commSpeed, timeout, flowControl) ) 
import System.Directory ( doesFileExist ) 
import System.Random ( newStdGen, Random(randoms) )
--
-- definitions for export
--
type Data807d   =
    (BS.ByteString,BS.ByteString)
type Event      =
    (LocalTime, EvContents)
data EvContents = PortNotFound FilePath
                | Connected ModelDataSet
                | OffLined
                | Tick RData
                | GotIACPos Int
                | Done String
                | Error String
                deriving Eq
-- | MEMS Commands
data UCommand = Disconnect | Init | Get807d | ClearFaults | RunFuelPump | StopFuelPump 
              | GetIACPos | IncIACPos | DecIACPos | IncIgAd | DecIgAd | TestActuator deriving (Eq,Show)
type RData    = Data807d
-- | Ecu model and its identical data
data ModelDataSet = ModelDataSet { name :: !String, d8size :: !Int, d7size :: !Int} deriving Eq
-- instance Show ModelDataSet where show  = show . name 
-- type Models = [(BS.ByteString , ModelDataSet)]
-- data ActuatorCmdDataSet = DefMinMax { def:: !Int,min:: !Int,max:: !Int} -- ^ (Default value, Minimum, Maximum)
-- fuelTrimDftMinMax  = DefMinMax 0x8a 0x00 0xfe  -- for MNE101170
-- idleDecayDftMinMax = DefMinMax 0x23 0x0a 0x3c  -- for MNE101170
-- idleSpeedDftMinMax = DefMinMax 0x80 0x78 0x88  -- for MNE101170
-- ignAdvOffDftMinMax = DefMinMax 0x80 0x74 0x8c  -- for MNE101170
-- | MEMS Monad related
data Env  = Env 
    { path  :: !FilePath
    , port  :: !SerialPort
    , model :: !ModelDataSet
    , dch   :: BC.BChan Event   -- ^ channel to inject original events for brick
    , cch   :: TChan UCommand -- ^ inlet channel to get user command for mems
    , lch   :: TChan Event
    , tickt :: ThreadId
    }
type MEMS = ReaderT Env IO
--
-- | main function
--
run ::  MEMS a -> (FilePath,BC.BChan Event,TChan UCommand,TChan Event) -> IO ()
run c r = Ex.bracket {- :: IO a	-> (a -> IO b) -> (a -> IO c)	-> IO c	 -}
  -- for resource acquisition ( opening )
  (Mems.init r) -- -> IO (Maybe Env)
  -- for resource releasoe ( closing )
  (\case       --  
      Nothing -> return () -- fail "Some error occured while running ecu."
      Just e' -> do
          flush $ port e'
          closeSerial $ port e'
          t <- currentTime
          let e = OffLined
          BC.writeBChan (dch e') (t,e)
          killThread $ tickt e'
      )
  -- for using resources
  (\case      -- Maybe Env
      Nothing  -> return () -- fail "ECU Initialization failed."
      Just env -> do {  threadDelay 10000 ; _ <- runReaderT c env ; return () } ) -- start loop
      -- threadDelay inserted on 10th April 2020 for testing wether or not having 
      -- effect to continuous connection for inital usstable term. K.UONO
--
loop :: MEMS ()
loop = do
    env <- ask
    let cmdchan = cch  env
        devfile = path env
        clearchan = do
          d <- atomically $ tryReadTChan cmdchan
          case d of
              Just _  -> clearchan
              Nothing -> return () 
    exist <- lift $ doesFileExist devfile
    if not exist 
      then do -- device file が存在しない（つまりUSBにRS232Cコンバータが接続されていなかった場合）
          report $ Error "Device Not Exist."
          return () -- ループから抜ける
      else do -- device file が存在していた場合（つまりUSBにRS232Cコンバータが接続されていた場合）
          c <- lift $ atomically $ readTChan cmdchan
          r <- res c -- do command on mems
          report r -- データチャネルおよびログに結果を書き込む
          case (c,r) of
            (Disconnect,_)       -> do { lift clearchan ; return () } -- 命令が切断だった場合，
            (_, PortNotFound _ ) -> do { lift clearchan ; return () }
            (_, OffLined )       -> do { lift clearchan ; return () }
            (_, Error _ )        -> do { lift clearchan ; return () }
            _                    -> loop
                -- つまり Init | Get807d | ClearFaults | RunFuelPump | StopFuelPump |
                --  GetIACPos | IncIACPos | DecIACPos | IncIgAd | DecIgAd | TestActuator
                -- のいずれかだった場合，        
--
report :: EvContents -> MEMS ()
report c = do
  t <- lift currentTime
  e <- ask
  let datach = dch e
      logch  = lch e
  lift $ BC.writeBChan datach (t,c)
  lift $ atomically $ writeTChan logch (t,c)
  return ()

--
-- | User Command achievement functions
res :: UCommand -> MEMS EvContents
res Init         = get807d -- ignore init command while engine is running
res Get807d      = get807d -- tick
res Disconnect   = offline
res ClearFaults  = clearFaults
res RunFuelPump  = runFuelPump
res StopFuelPump = stopFuelPump
res GetIACPos    = getIACPos
res IncIACPos    = incIACPos
res DecIACPos    = decIACPos
res IncIgAd      = incIgAd
res DecIgAd      = decIgAd
res _            = get807d -- 未実装のコマンドは無視する
-- | ecu 初期化関数　
init ::(FilePath,BC.BChan Event,TChan UCommand,TChan Event) -- ^ デバイスパス，UIイベント・送信・受信各チャネル
     -> IO (Maybe Env)
init (f,dc,cc,lc)= do
    -- putStrLn "Initializing started."
    threadDelay 1000000 {- 1sec delay -}
    exist <- doesFileExist f
    j <- currentTime
    if not exist 
        then do
            BC.writeBChan dc (j,PortNotFound f)
            -- atomically $ writeTChan lc (j,PortNotFound f) 未接続時にログが巨大化するためコメントアウト
            return Nothing
        else do
            sp   <- openSerial f defaultSerialSettings { commSpeed = CS9600, timeout= 1, flowControl = Software }
            --
            _   <- send sp $ BS.singleton (chr 0xca)  -- 202 'ha no hankaku' 
            _   <- tryRecv1Byte sp 5
            -- putStrLn $ "r1 = " ++ show r1 ++ ":" ++ show r1'
            _   <- send sp $ BS.singleton (chr 0x75)  -- 117 'u' 
            _   <- tryRecv1Byte sp 5
            -- putStrLn $ "r2 = " ++ show r2 ++ show r2'
            _   <- send sp $ BS.singleton $ fst htbt  -- 244 f4 -> f4 00
            _   <- tryRecv1Byte sp 5
            _   <- tryRecv1Byte sp 5
            _   <- send sp $ BS.singleton (chr 0xd0)  -- 208 'mi no hankaku'
            _   <- tryRecv1Byte sp 5
            m <- tryRecvNBytes sp BS.empty 4 -- モデルデータの読みとり
            if m == BS.empty || BS.length m /= 4
                then return Nothing
                else do
                    tt <- forkIO $ forever $ do -- 定期的にデータ送付を命令するループスレッドを立ち上げる
                        atomically $ writeTChan cc Get807d
                        threadDelay 400000 {- firing get807d frequency : every 0.4 sec -}
                    let m' = lookup m models  -- モデルデータを調べる
                    md <- case m' of
                        Nothing  -> do -- モデルデータが未知の場合，データを1セット読み出してデータ長を調べる。
                            test <- runReaderT get807d $ Env { path = f, port = sp, model = snd mneUnknown, dch = dc , cch = cc , lch = lc , tickt = tt } 
                            let (d8l,d7l) = case test of
                                  Tick (d8,d7) -> (ord $ BS.index d8 0,ord $ BS.index d7 0)
                                  _            -> (28,14)
                            return $ (snd mneUnknown) {name = "Unknown ( " ++ show d8l ++ ":" ++ show d7l ++ ")" ,d8size = d8l,d7size = d7l}
                        Just md' -> return $ md' {name = name md' ++ "(" ++ show (d8size md') ++ ":" ++ show (d7size md') ++ ")" } 
                    BC.writeBChan dc (j,Connected md)
                    atomically $ writeTChan lc (j,Connected md)
                    -- atomically $ writeTChan cc GetIACPos -- ^ 2020.01.11 追記
                    return $ Just Env { path = f, port = sp, model = md, dch = dc , cch = cc , lch = lc , tickt = tt } 
--
get807d :: MEMS EvContents
get807d =  do
  e  <- ask
  r8 <- sndCmd80
  case r8 of 
    Left  m    -> return $ Error m
    Right r8'  ->   
      if r8' == BS.empty then do
          lift $ flush $ port e -- 取りこぼし対策。
          return $ Error "Error in getting 80 data. Empty Response."
      else do
          r7 <- sndCmd7d
          case r7 of
            Left  m   -> return $ Error m
            Right r7' ->
              if r7' == BS.empty then do
                  liftIO $ flush $ port e -- 取りこぼし対策。
                  return $ Error "Error in getting 7d data. Empty Response."
              else
                  return $ Tick (r8',r7')
--
data Frame = Frame
  { d80size     :: !Int
  , engineSpeed :: !Int   -- 0x01-2	Engine speed in RPM (16 bits)
  , coolantTemp :: !Int   -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
  , ambientTemp :: !Int   -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
  , intakeATemp :: !Int   -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
  , fuelTemp    :: !Int   -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
  , mapSensor   :: !Int   -- 0x07	MAP sensor value in kilopascals
  , battVoltage :: !Float -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
  , ibattVoltage:: !Int
  , throttlePot :: !Float -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
  , ithrottlePot:: !Int
  , idleSwitch  :: !Bool  -- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
  , idleByte    :: !Int
  , unknown0B   :: !Int   -- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
  , pnClosed    :: !Int   -- 0x0C	Park/neutral switch. Zero is closed, nonzero is open.
      -- Fault codes. On the Mini SPi, only two bits in this location (1,2,10,16) are checked:
      -- ()=RoverMEMS FaultCode/[]=MiniMoni Error Num/Message, * は初期のインジェクション車によくフォルトが入るが異常ではない（キャメル）
  , faultCode1  :: !Bool  -- 0x0D  * Bit 0: Coolant temp sensor fault                              (Code 1) : [01/COOLANT]
  , faultCode2  :: !Bool  --       * Bit 1: Inlet air temp sensor fault                            (Code 2) : [02/Air TEMP]
  , faultCodeX4 :: !Bool  --       * Bit 4: Maybe Ambient air temp Sensor Error (But no installed on Mini)  : Maybe [03/ERROR 05]
  , faultCodeX5 :: !Bool  --       * Bit 5: Maybe Fuel Temp Sensor Error (But not installed on Mini)        : Maybe [04/ERROR 06]*
  , faultCode10 :: !Bool  -- 0x0E  * Bit 1: Fuel pump circuit fault                               (Code 10)
  , faultCodeY5 :: !Bool  --       * Bit 5: Maybe intake manifold pressure sesnor (MAP Sensor) fault        : Maybe [05/MAP SENS]
  , faultCode16 :: !Bool  --       * Bit 7: Throttle pot circuit fault                            (Code 16) : Maybe [06/T-POT]
      --                                                                                                                          : [07/T-POT PS]* [08/T-POT SU]* [09/CRANK NG]
  , faultCode0D :: !Int
  , faultCode0E :: !Int
  , unknown0F   :: !Int -- 0x0F	Unknown
  , unknown10   :: !Int -- 0x10	Unknown
  , unknown11   :: !Int -- 0x11	Unknown
  , idleACMP    :: !Int   -- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
  , idleSpdDev  :: !Int   -- 0x13-14	Idle speed deviation (16 bits)
  , unknown15   :: !Int -- 0x15	Unknown
  , ignitionAd  :: !Float   -- 0x16	Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
  , coilTime    :: !Float   -- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
  , unknown19   :: !Int  -- 0x19	Unknown
  , unknown1A   :: !Int  -- 0x1A	Unknown
  , unknown1B   :: !Int  -- 0x1B	Unknown
  , d7dsize     :: !Int
  , lambda_voltage:: !Int  -- This lambda value is a calculated value (if it is the same as the British emissions test).     And a value of, say, 1.05, suggests it is 5% too lean.   But, if your oxygen (and CO and HC) readings are all good, then it suggests your high lambda reading is because of a leak in the exhaust wgich pulls in fresh air (and oxygen).     You could try starting your car when it is cold and put your hand over the exhaust pipe and look underneath to see if water is leaking from any if the joints. 
  , closed_loop'  :: !Int  -- 0 : Open Loop, others : Closed Loop  
  , fuel_trim'    :: !Int  
  }
--
-- internal library
--
-- | ECU Commands
type Command  = (Char,String) -- ^ ECU returns echo and one result byte. Command byte (send to ECU), Num of Response following bytes from ECU
-- type Command' = (Char,String) -- ^ ECU returns only echo byte.
opnfp, opnpr, opnac, clspv, opnO2, clsfp, clspr, clsac, opnpv, clsO2, clsf1, clsf2, icrft, dcrft :: Command
icrft', dcrft', req7d, req80 , incid, decid, incis, decil, incia, decia :: Command
clrft, htbt, actfi, figcl, reqip, opiac, cliac, rqiac :: Command
opnfp = (chr 0x01,"Open Fuel Pump relay =stop"):: Command -- Open fuel pump relay (stop fuel pump) 
opnpr = (chr 0x02,"Open PTC Relay")            :: Command -- Open PTC relay (inlet manifold heater)
opnac = (chr 0x03,"Open A/C Relay")            :: Command -- Open air conditioning relay 
clspv = (chr 0x08,"Close purge valve?")        :: Command -- Close purge valve ?
opnO2 = (chr 0x09,"Open O2 heater relay?")     :: Command -- Open O2 heater relay ?
clsfp = (chr 0x11,"Close Fuel Pump relay =run"):: Command -- Close fuel pump relay (run fuel pump)
clspr = (chr 0x12,"Close PTC Relay")           :: Command -- Close PTC Relay (inlet manifold heater)
clsac = (chr 0x13,"Close A/C Relay")           :: Command -- Close air conditioning relay
opnpv = (chr 0x18,"Open purge valve?")         :: Command -- Open purge valve ?)
clsO2 = (chr 0x19,"Close O2 heater relay ?")   :: Command -- Close O2 heater relay ?
clsf1 = (chr 0x1d,"Close Fan 1 relay?")        :: Command -- Command' -- Close Fan 1 relay ? 
clsf2 = (chr 0x1e,"Close Fan 2 relay?")        :: Command -- Command' -- Close Fan 2 relay ?
icrft = (chr 0x79,"Increment Fuel Trim")       :: Command -- Increments fuel trim setting and returns the current value
dcrft = (chr 0x7a,"Decrement Fuel Trim")       :: Command -- Decrements fuel trim setting and returns the current value
icrft'= (chr 0x7b,"Increment Fuel Trim-2")     :: Command -- Increments fuel trim setting and returns the current value
dcrft'= (chr 0x7c,"Decrement Fuel Trim-2")     :: Command -- Decrements fuel trim setting and returns the current value 
req7d = (chr 0x7d,"Request data frame/7D")     :: Command -- get data for frame7d - followed by 32-byte data frame; 125
req80 = (chr 0x80,"Request data frame/80")     :: Command -- get data for frame80 - followed by 28-byte data frame; 128
incid = (chr 0x89,"Increments idle decay")     :: Command -- Increments idle decay setting and returns the current value
decid = (chr 0x8a,"Decrements idle decay")     :: Command -- Decrements idle decay setting and returns the current value
incis = (chr 0x91,"Increments idle speed")     :: Command -- Increments idle speed setting and returns the current value
decil = (chr 0x92,"Decrements idle speed")     :: Command -- Decrements idle speed setting and returns the current value
incia = (chr 0x93,"Increments ignition ad")    :: Command -- Increments ignition advance offset and returns the current value
decia = (chr 0x94,"Decrements ignition ad")    :: Command -- Decrements ignition advance offset and returns the current value
clrft = (chr 0xcc,"Clear fault code")          :: Command -- 204, Clear fault codes	CC 00
htbt  = (chr 0xf4,"NOP/heartbeat?")            :: Command -- 0xf4 244 NOP / heartbeat? Sent continuously by handheld diagnostic tools to verify serial link.
actfi = (chr 0xf7,"Actuate fuel incejtor")     :: Command -- F7 03 (SPI?)
figcl = (chr 0xf8,"Fire ignition coil")        :: Command -- F8 02 
reqip = (chr 0xfb,"Request IAC position")      :: Command -- FB xx where second byte represents the IAC position
opiac = (chr 0xfd,"Open IAC one and get pos")  :: Command -- FD xx, where the second byte represents the IAC position
cliac = (chr 0xfe,"Close IAC one and get pos") :: Command
rqiac = (chr 0xff,"Request current IAC pos?")  :: Command
--
-- Actuator Command
-- 
-- From [Bearinghead.com](http://www.bearinghead.com/car_stuff/mems_interface/)
--   Although some of the actuators have pairs of on/off commands to drive them,
--   I've found that the system fitted to the Mini SPi will actually turn off the
--   AC relay, PTC relay, and fuel pump relay automatically after a short time 
--   (i.e. without requiring the 'off' command). The 'off' command is acknowledged
--   by the ECU, but apparently has no effect.
fuelPumpOn, fuelPumpOff, ptcRelayOn, ptcRelayOff, acRelayOn, acRelayOff :: Command
testInjectors, fireCoil, openIac, closeIac :: Command
fuelPumpOn     = (chr 0x11,"Fuel Pump on")   :: Command -- 11 00
fuelPumpOff    = (chr 0x01,"Fuel Pump off")  :: Command -- 01 00
ptcRelayOn     = (chr 0x12,"ptc Relay on")   :: Command -- 12 00
ptcRelayOff    = (chr 0x02,"ptc Relay off")  :: Command -- 02 00
acRelayOn      = (chr 0x13,"a/c Relay on")   :: Command -- 13 00
acRelayOff     = (chr 0x03,"a/c Relay off")  :: Command -- 03 00
testInjectors  = (chr 0xf7,"test injectors") :: Command -- f7 03 (SPi ?)
fireCoil       = (chr 0xf8,"fire coil")      :: Command -- f8 02
openIac        = (chr 0xfd,"open IAC")       :: Command -- fd xx where the second byte represents the IAC position
closeIac       = (chr 0xfe,"close IAC")      :: Command -- fe xx where the second byte represents the IAC position
-- cfd = 0xcc ::Word8  -- 204
sndCmd80 :: MEMS (Either String BS.ByteString)
sndCmd80 = getData req80
-- 
sndCmd7d :: MEMS (Either String BS.ByteString)
sndCmd7d = getData req7d
-- | 
getData :: Command -> MEMS (Either String BS.ByteString)
getData c = 
  do
    e <- ask
    let c' = fst c
        p  = port e
    r  <- sendCommandAndGet1Byte p c'
    case r of
      Left  _  -> do { liftIO $ flush p ; return r }
      Right r' -> do
              let l = ord $ BS.index r' 0
              rs <- liftIO $ tryRecvNBytes p r' (l- 1)
              let l' = BS.length rs
              if rs == BS.empty 
                  then return $ Left $ "Continuous data was empty for " ++ show c
                  else if l' /= l 
                    then return $ Left $ "Continuous data length was wrong for " ++ show c ++ "(" ++ show l' ++ ":" ++ show l ++ ")"
                    else if l /= (if c == req80 then d8size (model e) else d7size (model e))
                        then return $ Left $ "Coutinuous data length was illegular for " ++ show c ++ "(" ++ show l' ++ ":" ++ show l ++ ")"
                        else return $ Right $ BS.append rs r'
--
offline :: MEMS EvContents
offline = do
    e <- ask
    let p = port e
    lift $ closeSerial p
    return OffLined
--
clearFaults :: MEMS EvContents
clearFaults = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst clrft
  liftIO $ flush p
  return $ case r of
    Left  m  -> Error $ "Clear Fault Error as :" ++ m
    Right r' -> if r' == BS.empty 
      then Error "Clear Fault Error."
      else Done  "Cleared Fault."
--
runFuelPump :: MEMS EvContents
runFuelPump = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst clsfp
  liftIO $ flush p
  return $ case r of
    Left  m  -> Error $ "Run fuel pump error as : " ++ m
    Right r' -> if r' == BS.empty
      then Error "Run fuel pump error."
      else Done  "Started fuel pump."
--
stopFuelPump :: MEMS EvContents
stopFuelPump = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst opnfp
  liftIO $ flush p
  return $ case r of
    Left  m  -> Error $ "Stop Fuel pump error as :" ++ m
    Right r' -> if r' == BS.empty
      then Error "Stop Fuel Pump error."
      else Done  "Stopped fuel pump."
--
getIACPos :: MEMS EvContents
getIACPos = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst reqip
  liftIO $ flush p
  return $ case r of
    Left  m  -> Error m
    Right r' -> if r' == BS.empty
      then Error "Get IAC Pos error."
      else GotIACPos $ ord (BS.index r' 0)
--
incIACPos :: MEMS EvContents
incIACPos = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst opiac
  lift $ flush p
  return $ case r of
    Left  m  -> Error $ "Increment IAC Position Error as : " ++ m
    Right r' -> if r' == BS.empty
      then Error "Increment IAC Pos error."
      else GotIACPos $ ord (BS.index r' 0)
--
decIACPos :: MEMS EvContents
decIACPos = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst cliac
  lift $ flush p
  return $ case r of
    Left  m  -> Error $ "Decrement IAC Position error as : " ++ m
    Right r' -> if r' == BS.empty
      then Error "Decrement IAC Pos error."
      else GotIACPos $ ord (BS.index r' 0)
--
incIgAd :: MEMS EvContents
incIgAd = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst incia
  lift $ flush p
  return $ case r of
    Left  m  -> Error $ "Increment Ignission advance error as : " ++ m
    Right r' -> if r' == BS.empty
      then Error "Increment Ignission ad error."
      else Done  "Incremented Ignission ad."
--
decIgAd :: MEMS EvContents
decIgAd = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst decia
  liftIO $ flush p
  return $ case r of
    Left  m  -> Error $ "Decrement Igmission advance error as : " ++ m
    Right r' -> if r' == BS.empty
      then Error "Decrement Ignission ad error."
      else Done  "Decremented Ignission ad."
--
-- Library functions
--
currentTime :: IO LocalTime
currentTime = do
  zone <- getCurrentTimeZone
  utcToLocalTime zone <$> getCurrentTime
-- | dummy frame data
dummyData807d :: IO Data807d
dummyData807d = do
  g <- newStdGen
  let r = randoms g :: [Char]
      r1 = Prelude.take 27 r 
      r2 = Prelude.take 31 $ Prelude.drop 28 r 
  return (BS.pack (chr 28:r1),BS.pack (chr 32:r2))-- random807d :: IO ECU.Data807d
--
-- dummyFrameData :: IO Frame
-- dummyFrameData = parse <$> dummyData807d
--
-- constants
--
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
-- mdata      = show . BS.unpack . mdb -- mapM (printf " %02X") . BS.unpack . mdb 
models :: [(BS.ByteString, ModelDataSet)]
models = [ mneUnknown, mneAuto, mne10078, mne101070, mne101170 ] -- 28 = 0x1c = \FS, 14 = 0x0. = \SO
mneUnknown, mneAuto, mne10078,mne101070,mne101170 :: (BS.ByteString, ModelDataSet)
mneUnknown = (BS.pack $ map chr [0x00,0x00,0x00,0x00], 
              ModelDataSet { name = "unknown                      ", d8size = 28 , d7size = 14 })
mneAuto    = (BS.pack $ map chr [0x3a, 0x00, 0x02, 0x14] {- 58,0,2,20 -}    ,
              ModelDataSet { name = "Japanese Automatic           ", d8size = 28 , d7size = 14 })
mne10078   = (BS.pack $ map chr [0x39, 0x00, 0x00, 0x5c] {-  57, 0, 0, 92 -},
              ModelDataSet { name = "MNE10078  M/SPI Japan Cooper ", d8size = 28 , d7size = 14 })
mne101070  = (BS.pack $ map chr [0x99, 0x00, 0x02, 0x03] {- 153, 0, 2,  3 -}, 
              ModelDataSet { name = "MNE101070 M/SPI Cooper       ", d8size = 28 , d7size = 32 })
mne101170  = (BS.pack $ map chr [0x99, 0x00, 0x03, 0x03] {- 153, 0, 3,  3 -},
              ModelDataSet { name = "MNE101170 M/SPI Except Cooper", d8size = 28 , d7size = 32 })
    -- https://blogs.yahoo.co.jp/dmxbd452/5751726.html
    -- http://www.minispares.com/product/Classic/MNE101070.aspx
--
mname :: ModelDataSet -> String
mname = name
--
-- library functions
--
parse :: Data807d -> Frame -- ここは何らかのParseライブラリを使い，可変長パラメタに対応したい
parse (d8,d7) =  {-# SCC "parse" #-} 
  let d8l = ord (BS.index d8 0)
      d7l = ord (BS.index d7 0)
      iv  = ord ( BS.index d8 8 ) -- 電圧値の10倍
      it  = 2 * ord ( BS.index d8 9 ) -- スロットルポテンションセンサー値の50倍
  in  Frame
      { d80size     = d8l
      , d7dsize     = d7l
      , engineSpeed = 256 * ord (BS.index d8 1) + ord (BS.index d8 2)
      , coolantTemp = -55 + ord (BS.index d8 3)  -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
      , ambientTemp = -55 + ord (BS.index d8 4)  -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
      , intakeATemp = -55 + ord (BS.index d8 5)  -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
      , fuelTemp    = -55 + ord (BS.index d8 6)  -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
      , mapSensor   = ord ( BS.index d8 7 )      -- 0x07	MAP sensor value in kilopascals
      , ibattVoltage = iv 
      , battVoltage  = 0.1 * fromIntegral iv     -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
      , ithrottlePot = it
      , throttlePot  = 0.01 * fromIntegral it    -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
      , idleSwitch  = testBit (ord $ BS.index d8 10) 4   -- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
      , idleByte    = ord $ BS.index d8 10
      , unknown0B   = ord $ BS.index d8 11        -- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
      , pnClosed    = ord $ BS.index d8 12        -- 0x0C	Park/neutral switch. Zero is closed, nonzero is open.
      -- ()=RoverMEMS FaultCode/[]=MiniMoni Error Num/Message, * は初期のインジェクション車によくフォルトが入るが異常ではない（キャメル）
      , faultCode1  = testBit (fromEnum $ BS.index d8 0x0d) 0   -- CTS : Coolant temp sensor circuit fault               (Code 1) : [01/COOLANT]
      , faultCode2  = testBit (fromEnum $ BS.index d8 0x0d) 1   -- ATS : Inlet Air temp sensor circuit fault             (Code 2) : [02/Air TEMP]
      , faultCodeX4 = testBit (fromEnum $ BS.index d8 0x0d) 4   -- Maybe Ambient air temp Sensor Error (But no installed on Mini) : Maybe [03/ERROR 05]
      , faultCodeX5 = testBit (fromEnum $ BS.index d8 0x0d) 5   -- Maybe Fuel Temp Sensor Error (But not installed on Mini)       : Maybe [04/ERROR 06]*
      , faultCode10 = testBit (fromEnum $ BS.index d8 0x0e) 1   -- Fuel pump circuit fault                              (Code 10)
      , faultCodeY5 = testBit (fromEnum $ BS.index d8 0x0e) 5   -- Maybe intake manifold pressure sesnor (MAP Sensor) fault       : Maybe [05/MAP SENS]
      , faultCode16 = testBit (fromEnum $ BS.index d8 0x0e) 7   -- TPS Throttle position sensor cuicuit fault           (Code 16) : Maybe [06/T-POT]
      --                                                                                                                          : [07/T-POT PS]* [08/T-POT SU]* [09/CRANK NG]
      , faultCode0D = ord $ BS.index d8 0x0d
      , faultCode0E = ord $ BS.index d8 0x0e
      , unknown0F   = ord $ BS.index d8 15               -- 0x0F	Unknown
      , unknown10   = ord $ BS.index d8 16               -- 0x10	Unknown
      , unknown11   = ord $ BS.index d8 17               -- 0x11	Unknown
      , idleACMP    = ord ( BS.index d8 18 ) -- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
      , idleSpdDev  = 256 * ord (BS.index d8 19) + ord (BS.index d8 20)  -- 0x13-14	Idle speed deviation (16 bits)
      , unknown15   = ord $ BS.index d8 21               -- 0x15	Unknown
      , ignitionAd  = -24.0 + 0.5 * fromIntegral  (ord $ BS.index d8 22)  -- 0x16	Ignition  0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
      , coilTime    = 0.02 * ( 256 * fromIntegral  (ord $ BS.index d8 23) + fromIntegral ( ord $ BS.index d8 24 ) ) -- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
      , unknown19   = ord $ BS.index d8 25               -- 0x19	Unknown
      , unknown1A   = ord $ BS.index d8 26               -- 0x1A	Unknown
      , unknown1B   = ord $ BS.index d8 27               -- 0x1B	Unknown
      , lambda_voltage = 5 * ord ( BS.index d7 0x06 )
      , closed_loop'   = ord ( BS.index d7 0x0a )
      , fuel_trim'     = ord ( BS.index d7 0x0c )
      } 
-- 
-- -- |Receive bytes, given the maximum number
-- recv :: SerialPort -> Int -> IO B.BS.ByteString
-- -- |Send bytes
-- send :: SerialPort -> B.BS.ByteString -> IO Int  -- ^ return Number of bytes actually sent
-- | an action to get 1 byte ECU response as a result of an ECU command. 
sendCommandAndGet1Byte :: SerialPort -> Char -> MEMS (Either String BS.ByteString)
sendCommandAndGet1Byte p c = do
  s <- lift $ send p $ BS.singleton c
  if s == 0 then
    return $ Left $ "The command ( " ++ show c ++ " ) was not sent." -- BS.empty
  else
    do
      r <- liftIO $ tryIO 5 $ recv p 1 -- get echo 
      -- liftIO $ flush p
      case r of
        Left  m  -> return $ Left $ m ++ " while waiting for echo byte ( " ++ show c ++ ")."
        Right r' -> if r' == BS.empty 
          then return $ Left $ "No Response 1 byte for " ++ show c -- BS.empty -- fail $ "ECU did not responsed while command ( " ++ show c ++ " ) was sent."
          else liftIO $ tryIO 5 $ recv p 1
-- | an action to try (IO BS.ByteString) Int times and returns BS.empty when some error occured. 
tryIO ::    Int              -- ^ number of times to try to do the action 
         -> IO BS.ByteString -- ^ the action which returns BS.empty when error occured
         -> IO (Either String BS.ByteString) 
tryIO n a -- try n times action
  | n <= 0    = return $ Left "The challenge of getting data failed"
  | otherwise = do
      r <- a 
      if r == BS.empty then do
        threadDelay 1000
        tryIO (n-1) a 
      else
        return $ Right r
--
tryRecvNBytes :: SerialPort -> BS.ByteString -> Int -> IO BS.ByteString
tryRecvNBytes ecuport !acc n =  
    if n <= 0
      then return acc
      else do
        r <- tryRecv1Byte ecuport 10
        if r == BS.empty
          then return acc
          else do threadDelay 1000 ; tryRecvNBytes ecuport (BS.append acc r) (n - 1)
-- | repeat n times to read 1 byte from ecu
tryRecv1Byte :: SerialPort
             -> Int              -- ^ times to try
             -> IO BS.ByteString
tryRecv1Byte p n
  | n <= 0    = return BS.empty
  | otherwise = do
      r <- recv p 1
      if r /= BS.empty then return r else tryRecv1Byte p (n-1)
--
-- toInt = fromIntegral . toInteger
-- Lirary
--
emptyD80, emptyD7d :: BS.ByteString
emptyD80 = BS.pack $ map chr [0x1c,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00] -- 28バイト
emptyD7d = BS.pack $ map chr [0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00] -- 32バイト
emptyData807d :: Data807d
emptyData807d = (emptyD80, emptyD7d)

-- data Loop        = OpenLoop | ClosedLoop deriving (Show)

