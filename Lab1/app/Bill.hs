module Bill (calculateBill, bill) where

bill :: Int -> Int -> Int -> Int
bill x y z =
    let total = x + y + z
        tip = total * 2
    in
        total + tip

calculateBill :: IO ()
calculateBill = do
    putStrLn "Enter the cost of the meal: "
    meal <- readLn
    putStrLn "Enter the cost of the drink: "
    drink <- readLn
    putStrLn "Enter the cost of the dessert: "
    dessert <- readLn
    let total = bill meal drink dessert
    putStrLn ("Total: " ++ show total)