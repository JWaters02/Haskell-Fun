module NITax (calculateNITax) where

lowerThreshold :: Int
lowerThreshold = 12584

lowerThresholdRateQ1_3 :: Float
lowerThresholdRateQ1_3 = 0.12

lowerThresholdRateQ4 :: Float
lowerThresholdRateQ4 = 0.1

upperThreshold :: Int
upperThreshold = 50284

upperThresholdRate :: Float
upperThresholdRate = 0.02

lowerThresholdDifference :: Int -> Float
lowerThresholdDifference income = fromIntegral (income - lowerThreshold)

middleThresholdDifference :: Float
middleThresholdDifference = fromIntegral (upperThreshold - lowerThreshold)

upperThresholdDifference :: Int -> Float
upperThresholdDifference income = fromIntegral (income - upperThreshold)

calculateNITax :: Int -> Float
calculateNITax income
    | income < lowerThreshold = 0
    | income < upperThreshold = (lowerThresholdDifference income * lowerThresholdRateQ1_3 * 0.75) + (lowerThresholdDifference income * lowerThresholdRateQ4 * 0.25)
    | otherwise = (middleThresholdDifference * lowerThresholdRateQ1_3 * 0.75) + (middleThresholdDifference * lowerThresholdRateQ4 * 0.25) + (upperThresholdDifference income * upperThresholdRate)