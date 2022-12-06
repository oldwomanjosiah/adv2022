module Lib(
    days,
    DynDay
) where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import Lib.Days

data DynDay = forall d. Day d => MkDynDay d
data DynShow = forall s. Show s => MkDynShow s

instance Day (DynDay) where
    type Result1 DynDay = DynShow
    type Result2 DynDay = DynShow

    name (MkDynDay d) = name d
    index (MkDynDay d) = index d

    part1 (MkDynDay d) s = fmap MkDynShow $ part1 d s
    part2 (MkDynDay d) s = fmap MkDynShow $ part2 d s

instance Show (DynShow) where
    show (MkDynShow s) = show s

pack :: forall d. Day d => d -> DynDay
pack = MkDynDay

days :: [DynDay]
days = [
        pack Day1.task,
        pack Day2.task,
        pack Day3.Task,
        pack Day4.Task,
        pack Day5.Task
    ]

