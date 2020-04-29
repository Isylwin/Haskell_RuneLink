module RuneLink.Base.Util
(
    next
) where

next :: (Eq a, Enum a, Bounded a) => a -> a
next d
    | d == maxBound = minBound
    | otherwise = succ d

