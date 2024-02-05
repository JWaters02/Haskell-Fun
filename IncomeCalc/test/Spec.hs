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
niTaxTests = TestList [
    TestCase (assertEqual "NI tax below threshold" 0 (calculateNITax 8000)),
    TestCase (assertEqual "NI tax at middle threshold" 852.84 (calculateNITax 20000)),
    TestCase (assertEqual "NI tax above threshold" 4529.82 (calculateNITax 60000))
    ]

incomeTaxTests :: Test
incomeTaxTests = TestList []