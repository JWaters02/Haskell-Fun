module StudentLoan (calculateStudentLoan) where

threshold :: Int
threshold = 27295

calculateStudentLoan :: Int -> Float
calculateStudentLoan income = if income > threshold then fromIntegral (income - threshold) * 0.09 else 0