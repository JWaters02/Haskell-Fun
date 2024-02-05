import Test.HUnit

import CalculateNetIncome
import IncomeTax
import NITax
import StudentLoan

main :: IO ()
main = do
    runTestTT allTests
    return ()

allTests :: Test
allTests = TestList [
    calculateNetIncomeTests,
    niTaxTests,
    incomeTaxTests,
    studentLoanTests,
    personalAllowanceTests,
    taxableIncomeTests
    ]

calculateNetIncomeTests :: Test
calculateNetIncomeTests = TestList [
    TestCase (assertEqual "Net income zero" 0 (calculateNetIncome 0)),
    TestCase (assertEqual "Net income basic rate below threshold" 10000 (calculateNetIncome 10000)),
    TestCase (assertEqual "Net income basic rate betweem thresholds" 21086.16 (calculateNetIncome 25000)),
    TestCase (assertEqual "Net income higher rate between thresholds" 30217.71 (calculateNetIncome 40000)),
    TestCase (assertEqual "Net income higher rate above threshold" 41094.73 (calculateNetIncome 60000)),
    TestCase (assertEqual "Net income additional rate above threshold" 100923.73 (calculateNetIncome 200000))
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

taxableIncomeTests:: Test
taxableIncomeTests = TestList [
    TestCase (assertEqual "Taxable income below threshold" 0 (calculateTaxableIncome 5000)),
    TestCase (assertEqual "Taxable income within threshold" 12430 (calculateTaxableIncome 25000)),
    TestCase (assertEqual "Taxable income above threshold" 117430 (calculateTaxableIncome 120000)),
    TestCase (assertEqual "Taxable income well above threshold" 130000 (calculateTaxableIncome 130000))
    ]

incomeTaxTests :: Test
incomeTaxTests = TestList [
    TestCase (assertEqual "Income tax below threshold" 0 (calculateIncomeTax 5000)),
    TestCase (assertEqual "Income tax at basic rate" 3486 (calculateIncomeTax 30000)),
    TestCase (assertEqual "Income tax at higher rate" 11432 (calculateIncomeTax 60000)),
    TestCase (assertEqual "Income tax at additional rate" 76203 (calculateIncomeTax 200000))
    ]