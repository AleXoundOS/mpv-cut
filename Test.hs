{-# LANGUAGE OverloadedStrings #-}
module Test where
import MPV_Cut

list :: [TimeStamp]
list = [ TimeStamp A "0.23"
       , TimeStamp A "0.6"
       , TimeStamp B "1.0"
       , TimeStamp B "1.5"
       , TimeStamp X "1.6"
       , TimeStamp B "1.7"
       , TimeStamp A "2.0"
       , TimeStamp B "8.0"
       ]

test :: [(TimeStamp,TimeStamp)]
test = MPV_Cut.firstCitizens list

test2 :: [(TimeStamp,TimeStamp)]
test2 = MPV_Cut.firstClassPieces list
