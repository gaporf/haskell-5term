{-# LANGUAGE InstanceSigs #-}

module Days
  (
    Day (Mon, Tue, Wed, Thu, Fri, Sat, Sun)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

-- | Day representation
data Day
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

instance Show Day where
  show
    :: Day
    -> String
  show Mon = "Monday"
  show Tue = "Tuesday"
  show Wed = "Wednesday"
  show Thu = "Thursday"
  show Fri = "Friday"
  show Sat = "Saturday"
  show Sun = "Sunday"

instance Eq Day where
  (==)
    :: Day
    -> Day
    -> Bool
  (==) x y = (show x) == (show y)

-- | Return a day in n days after input day
afterDays
  :: Day  -- ^ The input day
  -> Int  -- ^ The number of days after input day
  -> Day  -- ^ The day in n days after input day
afterDays x 0 = x
afterDays x y = nextDay $ afterDays x $ y - 1

-- | Count the number of days to Friday
daysToParty
  :: Day  -- ^ The input day
  -> Int  -- ^ Days to Friday
daysToParty Fri = 0
daysToParty day = 1 + (daysToParty $ nextDay day)

-- | Determine if input day is weekend
isWeekend
  :: Day   -- ^ The input day
  -> Bool  -- ^ True if input day is weekend, false otherwise
isWeekend day
    | day == Sat = True
    | day == Sun = True
    | otherwise  = False

-- | Return a next day
nextDay
  :: Day  -- ^ The input day
  -> Day  -- ^ The next day after input day
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon
