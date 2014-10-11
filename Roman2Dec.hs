import Data.List
import Data.Maybe
import Text.Regex.PCRE
import System.Environment (getArgs)

romanToDecimal :: String -> Maybe(Int)
romanToDecimal roman = dec where
  re = "^\\s*"
       ++ "(((MMMM)|(MMM)|(MM)|(M))?)"
       ++ "(((CM)|(CD)|(DCCC)|(DCC)|(DC)|(D)|((CCC)|(CC)|(C)))?)"
       ++ "(((XC)|(XL)|(LXXX)|(LXX)|(LX)|(L)|((XXX)|(XX)|(X)))?)"
       ++ "(((IX)|(IV)|(VIII)|(VII)|(VI)|(V)|((III)|(II)|(I)))?)"
       ++ "\\s*$"
  -- Values assigned to embedded matches.
  val = [0, 0, 4000, 3000, 2000, 1000,
         0, 0,  900,  400,  800,  700, 600, 500, 0, 300, 200, 100,
         0, 0,   90,   40,   80,   70,  60,  50, 0,  30,  20,  10,
         0, 0,    9,    4,    8,    7,   6,   5, 0,   3,   2,   1]
  -- Match against passed roman numeral string.
  (prefix, matched, unmatched, matches) = (roman =~ re :: (String, String, String, [String]))
  dec = if (null matched)
        then Nothing
        else Just (sum (map (\x -> (fst x) * (snd x)) (zip (map (\x -> if (null x) then 0 else 1) matches) val)))

main = mainWith
  where mainWith = do
          args <- getArgs
          case args of
            [arg] -> putStrLn (show (fromJust (romanToDecimal arg)))
            _ -> putStrLn "error: exactly one argument needed"
