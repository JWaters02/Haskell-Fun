module StudentLoan (calculateStudentLoan) where

calculateStudentLoan :: Int -> Double
calculateStudentLoan x = if x > 27295 then fromIntegral (x - 27295) * 0.09 else 0