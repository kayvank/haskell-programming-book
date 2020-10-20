-- |

module Calculator where
import           Exp
import           Parsers

buttons :: String
buttons = statndard ++ extra where
  statndard = "qcd=123+456-789*0()/"
  extra     = "QCD \ESC\BS\DEL\n"

  box :: [String]
  box =
    [ "+---------------+"
    , "|               |"
    , "+---------------+"
    , "| q | c | d | = |"
    , "| 1 | 2 | 3 | + |"
    , "| 1 | 2 | 6 | - |"
    , "| 7 | 8 | 9 | * |"
    , "| 0 | ( | ) | / |"
    , "+---------------+"
    ]
