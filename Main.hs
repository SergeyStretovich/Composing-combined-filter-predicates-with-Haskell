import Data.Monoid
import Data.Char


isSign :: Char -> Bool
isSign input
         | input == '.' = True
         | input == ':' = True
         | input == ';' = True
         | input == '!' = True
         | input == '?' = True
         | input == ',' = True
         | input == '-' = True
         | input == '+' = True
         | input == '_' = True
         | otherwise  = False


isForbidden :: Char -> Bool
isForbidden = getAny . foldMap (Any .) predicates
    where predicates =[isSign,isDigit,isLower]

isEitherM3OrM5 :: Int -> Bool
isEitherM3OrM5 = getAny . mconcat [
    \x -> Any (x `mod` 3 == 0),
    \x -> Any (x `mod` 5 == 0)
    ]

main :: IO ()
main = do
  let flag = isForbidden 'J'
  let flag1 = any isForbidden "trust_no_one"
  let flag' = any isForbidden "HereWeGoAgain"
  let flag2 = isEitherM3OrM5 7
  let flag3 = isEitherM3OrM5 15
  let flag4 = isEitherM3OrM5 11
  putStrLn (show flag)
  putStrLn (show flag1)
  putStrLn (show flag')
  putStrLn (show flag2)
  putStrLn (show flag3)
  putStrLn (show flag4)

  {-
  Composing combined filter predicates
  https://www.reddit.com/r/haskell/comments/citx4u/composing_combined_filter_predicates/
  -}