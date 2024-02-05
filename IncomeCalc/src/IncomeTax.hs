module IncomeTax (calculateIncomeTax, calculatePersonalAllowance, calculateTaxableIncome) where

calculatePersonalAllowance :: Int -> Float
calculatePersonalAllowance x = 
    if x > 100000
        then max 0 (12570 - fromIntegral (x - 100000) * 0.5)
        else 12570

calculateTaxableIncome :: Int -> Int
calculateTaxableIncome x = 
    max 0 (x - round (calculatePersonalAllowance x))

calculateIncomeTax :: Int -> Float
calculateIncomeTax x = 
    let taxableIncome = calculateTaxableIncome x
    in if taxableIncome <= 37700 
        then fromIntegral taxableIncome * 0.2
        else if taxableIncome <= 125140
            then (37700 * 0.2) + (fromIntegral (taxableIncome - 37700) * 0.4)
            else (37700 * 0.2) + (87440 * 0.4) + (fromIntegral (taxableIncome - 125140) * 0.45)