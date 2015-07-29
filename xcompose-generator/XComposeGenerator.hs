{-# LANGUAGE PatternGuards #-}
import Data.List (intercalate)
import Control.Monad (liftM)
import System.Environment (getArgs)

import XComposeGen.Utils (Table, keyFromChar, disamb)
import XComposeGen.Letterlike (letterlike)
import XComposeGen.Symbols (symbols)


main :: IO ()
main = liftM head getArgs >>= flip writeFile (process bindings)

bindings :: Table
bindings = concat [letterlike, symbols]

process :: Table -> String
process = unlines . map genLine . disamb
  where genLine (name, target) = "<Multi_key> " ++ keysFromString name ++ " : \"" ++ target ++ "\""
          where keysFromString = intercalate " " . map keyFromChar

