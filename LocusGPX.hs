{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module LocusGPX where

import Text.XML.Light
import qualified GH

qual :: String -> String -> QName
qual prefix x = (unqual x) { qPrefix = Just prefix }

pathToGPX :: GH.Path -> String
pathToGPX p = showTopElement $ gpxTop $ wpts ++ [trk]
    where
        wpts = map gpxWpt (GH.pInstrs p)
        trk = gpxTrk (GH.pPoints p)

gpxTop :: [Element] -> Element
gpxTop content = unode "gpx" (attrs, content)
    where
        attrs =
            [ Attr (unqual "version") "1.1"
            , Attr (unqual "creator") "locus-graphhopper-gpx"
            , Attr (unqual "xmlns") "http://www.topografix.com/GPX/1/1"
            , Attr (qual "xmlns" "xsi") "http://www.w3.org/2001/XMLSchema-instance"
            , Attr (qual "xsi" "schemaLocation") "http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"
            , Attr (qual "xmlns" "locus") "http://www.locusmap.eu"
            ]

lnode :: Node t => String -> t -> Element
lnode s x = node (qual "locus" s) x

gpxWpt :: GH.Instruction -> Element
gpxWpt i = unode "wpt" (latLon, [name, ext])
    where
        (lat, lon) = GH.iLocation i
        latLon = [ Attr (unqual "lat") (show lat), Attr (unqual "lon") (show lon) ]
        name = unode "name" $ GH.iText i
        ext = unode "extensions" exts
        dist = GH.iDistance i
        time = GH.iTime i
        exts =
            [ lnode "rteDistance" (show dist) ] ++
            [ lnode "rteTime" (show time) | time /= 0 ] ++
            [ lnode "rteSpeed" (show $ dist / time) | time /= 0 ] ++
            [ lnode "rtePointAction" (show $ pointAction $ GH.iSign i) ]

pointAction :: GH.InstrSign -> Int
pointAction GH.TurnSharpLeft = 5
pointAction GH.TurnLeft = 4
pointAction GH.TurnSlightLeft = 3
pointAction GH.ContinueOnStreet = 2
pointAction GH.TurnSlightRight = 6
pointAction GH.TurnRight = 7
pointAction GH.TurnSharpRight = 8
pointAction GH.Finish = 24
pointAction GH.ReachedVia = 50
pointAction GH.UseRoundabout{ GH.roundExitNumber = Just 1 } = 27
pointAction GH.UseRoundabout{ GH.roundExitNumber = Just 2 } = 28
pointAction GH.UseRoundabout{ GH.roundExitNumber = Just 3 } = 29
pointAction GH.UseRoundabout{ GH.roundExitNumber = Just 4 } = 30
pointAction GH.UseRoundabout{ GH.roundExitNumber = Just 5 } = 31
pointAction GH.UseRoundabout{ GH.roundExitNumber = Just 6 } = 32
pointAction GH.UseRoundabout{ GH.roundExitNumber = Just 7 } = 33
pointAction GH.UseRoundabout{ GH.roundExitNumber = Just 8 } = 34
pointAction s = error $ "pointAction: " ++ show s ++ " unsupported"

gpxTrk :: [GH.Point] -> Element
gpxTrk ps = unode "trk" [name, ext, gpxTrkSeg ps]
    where
        name = unode "name" "GraphHopper route"
        ext = unode "extensions" exts
        exts = lnode "rteComputeType" "-1"

gpxTrkSeg :: [GH.Point] -> Element
gpxTrkSeg ps = unode "trkseg" $ map gpxTrkPt ps

gpxTrkPt :: GH.Point -> Element
gpxTrkPt (lat, lon) = unode "trkpt" latLon
    where
        latLon = [ Attr (unqual "lat") (show lat), Attr (unqual "lon") (show lon) ]
