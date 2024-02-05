module IncomeTax (calculateIncomeTax, calculatePersonalAllowance, calculateTaxableIncome) where

fullPersonalAllowance :: Int
fullPersonalAllowance = 12570

incomeReductionStart :: Int
incomeReductionStart = 100000

personalAllowanceReductionRate :: Float
personalAllowanceReductionRate = 0.5

basicRateUpperLimit :: Int
basicRateUpperLimit = 37700

higherRateUpperLimit :: Int
higherRateUpperLimit = 125140

basicTaxRate :: Float
basicTaxRate = 0.2

higherTaxRate :: Float
higherTaxRate = 0.4

additionalTaxRate :: Float
additionalTaxRate = 0.45

calculatePersonalAllowance :: Int -> Float
calculatePersonalAllowance x = 
    if x > incomeReductionStart
        then max 0 (fromIntegral fullPersonalAllowance - fromIntegral (x - incomeReductionStart) * personalAllowanceReductionRate)
        else fromIntegral fullPersonalAllowance

calculateTaxableIncome :: Int -> Int
calculateTaxableIncome x = 
    max 0 (x - round (calculatePersonalAllowance x))

calculateIncomeTax :: Int -> Float
calculateIncomeTax x = 
    let taxableIncome = calculateTaxableIncome x
    in if taxableIncome <= basicRateUpperLimit 
        then fromIntegral taxableIncome * basicTaxRate
        else if taxableIncome <= higherRateUpperLimit
            then (fromIntegral basicRateUpperLimit * basicTaxRate) + 
                 (fromIntegral (taxableIncome - basicRateUpperLimit) * higherTaxRate)
            else (fromIntegral basicRateUpperLimit * basicTaxRate) + 
                 (fromIntegral (higherRateUpperLimit - basicRateUpperLimit) * higherTaxRate) + 
                 (fromIntegral (taxableIncome - higherRateUpperLimit) * additionalTaxRate)