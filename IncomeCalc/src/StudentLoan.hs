module StudentLoan (calculateStudentLoan) where

calculateStudentLoan :: Int -> Int
calculateStudentLoan x = if x > 25000 then x * 9 `div` 100 else 0