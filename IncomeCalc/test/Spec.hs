import Test.HUnit

import IncomeTax
import NITax
import StudentLoan (calculateStudentLoan)

main :: IO ()
main = do
    runTestTT allTests
    return ()

allTests :: Test
allTests = TestList [
    niTaxTests,
    incomeTaxTests,
    studentLoanTests
    ]

studentLoanTests :: Test
studentLoanTests = TestList [
    TestCase (assertEqual "Student loan below threshold" 0 (calculateStudentLoan 17000)),
    TestCase (assertEqual "Student loan above threshold" 243.45 (calculateStudentLoan 30000))
    ]

niTaxTests :: Test
niTaxTests = TestList []

incomeTaxTests :: Test
incomeTaxTests = TestList []