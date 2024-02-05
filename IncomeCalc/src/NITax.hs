module NITax (calculateNITax) where

calculateNITax :: Int -> Float
calculateNITax x = 
    if x < 12584
        then 0
        else if x < 50284
            then (fromIntegral (x - 12584) * 0.12 * 0.75) +  (fromIntegral (x - 12584) * 0.1 * 0.25)
            else (fromIntegral (50284 - 12584) * 0.12 * 0.75) +  (fromIntegral (50284 - 12584) * 0.1 * 0.25) + (fromIntegral (x - 50284) * 0.02)