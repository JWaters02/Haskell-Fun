module CalculateNetIncome (calculateNetIncome) where

import IncomeTax
import NITax
import StudentLoan

calculateNetIncome :: Int -> Float
calculateNetIncome income = 
    let incomeTax = calculateIncomeTax income
        niTax = calculateNITax income
        studentLoan = calculateStudentLoan income
    in fromIntegral income - incomeTax - niTax - studentLoan