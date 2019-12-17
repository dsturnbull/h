module CPU.Segment
  (Segment(..))
  where

data Segment = CodeSegment | DataSegment
  deriving (Show, Eq)
