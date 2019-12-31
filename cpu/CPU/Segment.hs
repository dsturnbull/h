module CPU.Segment
  (Segment(..))
  where

import Data.Word

data Segment = CodeSegment | DataSegment | OffsetSegment Word16
  deriving (Show, Eq)
