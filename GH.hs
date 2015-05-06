{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GH
    ( decodeJSON
    , Latitude, Longitude, Point
    , InstrSign(..), Instruction(..), Path(..)
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import GPolyline
import qualified Data.ByteString.Lazy as B

newtype InstrJSONInterval = InstrJSONInterval (Int, Int) deriving Show

instance FromJSON InstrJSONInterval where
    parseJSON v = do
        v' <- parseJSON v
        case v' of
            [a, b] -> return $ InstrJSONInterval (a, b)
            x -> fail $ "bad interval: " ++ show x

data InstrSign = LeaveRoundabout | TurnSharpLeft | TurnLeft | TurnSlightLeft
    | ContinueOnStreet | TurnSlightRight | TurnRight | TurnSharpRight | Finish
    | ReachedVia
    | UseRoundabout { roundExitNumber :: Maybe Int, roundTurnAngle :: Maybe Double }
    deriving Show

instance FromJSON InstrSign where
    parseJSON v = do
        v' <- parseJSON v
        case v' :: Int of
            -6 -> return LeaveRoundabout
            -3 -> return TurnSharpLeft
            -2 -> return TurnLeft
            -1 -> return TurnSlightLeft
            0 -> return ContinueOnStreet
            1 -> return TurnSlightRight
            2 -> return TurnRight
            3 -> return TurnSharpRight
            4 -> return Finish
            5 -> return ReachedVia
            6 -> return UseRoundabout{ roundExitNumber = Nothing, roundTurnAngle = Nothing }
            x -> fail $ "bad sign: " ++ show x

data InstrJSON = InstrJSON
    { ijText :: String
    , ijDistance :: Double -- meters
    , ijTime :: Double -- seconds
    , ijInterval :: InstrJSONInterval
    , ijSign :: InstrSign
    , ijExitNumber :: Maybe Int
    , ijTurnAngle :: Maybe Double
    } deriving Show

instance FromJSON InstrJSON where
    parseJSON (Object v) = do
        InstrJSON <$> v .: "text" <*> v .: "distance" <*> fmap (/ 1000) (v .: "time") <*> v .: "interval"
                  <*> v .: "sign" <*> v .:? "exit_number" <*> v .:? "turn_angle"
    parseJSON _ = mzero

type Latitude = Double
type Longitude = Double
type Point = (Latitude, Longitude)

newtype Polyline = Polyline { unPolyline :: [Point] } deriving Show

instance FromJSON Polyline where
    parseJSON = fmap (Polyline . decodeline) . parseJSON

data PathJSON = PathJSON
    { pjInstrs :: [InstrJSON]
    , pjPoints :: Polyline
    } deriving Show

instance FromJSON PathJSON where
    parseJSON (Object v) = do
        PathJSON <$> v .: "instructions" <*> v .: "points"
    parseJSON _ = mzero

data RouteJSON = RouteJSON { rjPaths :: [PathJSON] } deriving Show

instance FromJSON RouteJSON where
    parseJSON (Object v) = do
        RouteJSON <$> v .: "paths"
    parseJSON _ = mzero

data Instruction = Instruction
    { iText :: String
    , iDistance :: Double -- meters
    , iTime :: Double -- seconds
    , iSign :: InstrSign
    , iLocation :: Point
    } deriving Show

data Path = Path
    { pInstrs :: [Instruction]
    , pPoints :: [Point]
    } deriving Show

pathFromJSON :: PathJSON -> Path
pathFromJSON pj = Path{ pInstrs = is, pPoints = ps }
    where
        ps = unPolyline $ pjPoints pj
        is = map (instrFromJSON ps) (pjInstrs pj)

instrFromJSON :: [Point] -> InstrJSON -> Instruction
instrFromJSON ps ij = Instruction
    { iText = ijText ij, iDistance = ijDistance ij, iTime = ijTime ij
    , iSign = s, iLocation = ps !! i }
    where
        InstrJSONInterval (i, _) = ijInterval ij
        s = case ijSign ij of
            UseRoundabout{} -> UseRoundabout{ roundExitNumber = ijExitNumber ij
                                            , roundTurnAngle = ijTurnAngle ij }
            x -> x

decodeJSON :: B.ByteString -> Either String [Path]
decodeJSON s = do
    RouteJSON{ rjPaths = ps } <- eitherDecode s
    return $ map pathFromJSON ps
