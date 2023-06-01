import Test.QuickCheck
import Utils (strToInt)

test_strToInt :: Int -> Bool
test_strToInt num = strToInt (show num) == num

main = quickCheck test_strToInt
