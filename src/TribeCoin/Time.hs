module TribeCoin.Time
  (getCurrentTimsetamp
  , diffTimestamps
  ) where

import TribeCoin.Types (Timestamp (..), TimestampDiff (..))

import Data.Time (diffUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)

-- | Get the current timestamp from the system time.
getCurrentTimsetamp :: IO Timestamp
getCurrentTimsetamp = Timestamp <$> getPOSIXTime

diffTimestamps :: Timestamp -> Timestamp -> TimestampDiff
diffTimestamps (Timestamp timea) (Timestamp timeb) =
  let utca = posixSecondsToUTCTime timea
      utcb = posixSecondsToUTCTime timeb
  in TimestampDiff $ diffUTCTime utca utcb
