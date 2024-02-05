import Test.HUnit

import IncomeTax
import NITax
import StudentLoan

main :: IO ()
main = do
    runTestTT allTests
    return ()

allTests :: Test
allTests = TestList [
    niTaxTests,
    incomeTaxTests,
    studentLoanTests,
    personalAllowanceTests
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

personalAllowanceTests :: Test
personalAllowanceTests = TestList [
    TestCase (assertEqual "Personal allowance below threshold" 12570 (calculatePersonalAllowance 50000)),
    TestCase (assertEqual "Personal allowance above threshold" 2570 (calculatePersonalAllowance 120000)),
    TestCase (assertEqual "Personal allowance well above threshold" 0 (calculatePersonalAllowance 130000))
    ]

incomeTaxTests :: Test
incomeTaxTests = TestList [
    TestCase (assertEqual "Income tax below threshold" 0 (calculateIncomeTax 5000)),
    TestCase (assertEqual "Income tax at basic rate" 3486 (calculateIncomeTax 30000)),
    TestCase (assertEqual "Income tax at higher rate" 11432 (calculateIncomeTax 60000)),
    TestCase (assertEqual "Income tax at additional rate" 76203 (calculateIncomeTax 200000))
    ]