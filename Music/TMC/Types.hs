-- | Be safe kids, use newtypes

module Music.TMC.Types(
      BPM(..)
    , bpmRatio
    , beatLen
    , Duration(..)
    , durationAdd
    , durationDiff
    , durationTimes
    , Frequency(..)
    , Gain(..)
    )
    where

-- | Beats Per Minute.
newtype BPM = BPM Double
    deriving (Show)

-- | Ratio of two BPMs.
bpmRatio :: BPM -> BPM -> Double
bpmRatio (BPM a) (BPM b) =
    a / b

-- | Duration of a beat.
beatLen :: BPM -> Duration
beatLen (BPM bpm) =
    Duration $ 60 / bpm

-- | A duration, in seconds.
newtype Duration = Duration Double
    deriving (Show)

-- | Add two Durations.
durationAdd :: Duration -> Duration -> Duration
durationAdd (Duration a) (Duration b) =
    Duration $ a + b

-- | Difference between two Durations.
durationDiff :: Duration -> Duration -> Duration
durationDiff (Duration a) (Duration b) =
    Duration $ a - b

-- | Multiply an integer and a Duration.
durationTimes :: Int -> Duration -> Duration
durationTimes n (Duration a) =
    Duration $ fromIntegral n * a
-- | A frequency, in Hertz.
newtype Frequency = Frequency Double
    deriving (Show)

-- | A gain, in DB.
newtype Gain = Gain Double
