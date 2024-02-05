import Test.HUnit

import IncomeTax
import NITax
import StudentLoan

main :: IO ()
main = do
    runTestTT allTests
    return ()

allTests :: Test
allTests = TestList []