{-# LANGUAGE OverloadedStrings #-}
module Test where
import Data.List
import qualified Data.ByteString.Lazy

import MPV_Cut

list :: [TimeStamp]
list = [ TimeStamp A "0.23"
       , TimeStamp A "0.6"
       , TimeStamp B "1.0"
       , TimeStamp B "1.5"
       , TimeStamp X "1.6"
       , TimeStamp B "1.7"
       , TimeStamp A "2.0"
       , TimeStamp A "2.1"
       , TimeStamp B "8.0"
       , TimeStamp B "9.0"
       , TimeStamp B "9.1"
       , TimeStamp B "9.2"
       ]

list2 :: [TimeStamp]
list2 = [ TimeStamp B "0.45"
        , TimeStamp B "0.9"
        , TimeStamp B "1.5"
        , TimeStamp A "1.7"
        , TimeStamp B "1.8"
        , TimeStamp A "1.9"
        ]

test :: [(TimeStamp,TimeStamp)]
test = MPV_Cut.firstCitizens list

remaining :: [TimeStamp]
remaining = list \\ (tuplesToList $ nativeCitizens list)

test2 :: [(TimeStamp,TimeStamp)]
test2 = allPieces list

test3 :: [(TimeStamp,TimeStamp)]
test3 = allPieces list2

test4 :: Data.ByteString.Lazy.ByteString
test4 = bstrPieces $ allPieces list
