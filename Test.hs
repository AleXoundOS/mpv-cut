{-# LANGUAGE OverloadedStrings #-}
module Test where
import Data.List

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

test :: [(TimeStamp,TimeStamp)]
test = MPV_Cut.firstCitizens list

remaining :: [TimeStamp]
remaining = list \\ (tuplesToList $ allNativeCitizens list)

test2 :: [(TimeStamp,TimeStamp)]
test2 = adoptees remaining list
