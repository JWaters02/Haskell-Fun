module IncomeTax (calculateIncomeTax, calculatePersonalAllowance, calculateTaxableIncome) where

calculatePersonalAllowance :: Int -> Float
calculatePersonalAllowance x = 
    if x > 100000
        then max 0 (12570 - fromIntegral (x - 100000) * 0.5)
        else 12570

calculateTaxableIncome :: Int -> Int
calculateTaxableIncome x = 
    max 0 (x - round (calculatePersonalAllowance x))

calculateIncomeTax :: Int -> Int
calculateIncomeTax x = 0