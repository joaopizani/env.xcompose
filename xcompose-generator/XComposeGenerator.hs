{-# LANGUAGE PatternGuards #-}
import Data.Char (isAscii, isAlphaNum, ord)
import Data.List (intercalate, isPrefixOf)
import Control.Monad (liftM)
import System.Environment (getArgs)
import XComposeGen.Unicode (greek, subscripts, superscripts, symbols)


main :: IO ()
main = liftM head getArgs >>= flip writeFile (process bindings)

bindings :: [(String, String)]
bindings = concat [greek, subscripts, superscripts, symbols]

process :: [(String, String)] -> String
process = unlines . map genLine . disamb
  where genLine (name, target) = "<Multi_key> " ++ keysFromString name ++ " : \"" ++ target ++ "\""
          where keysFromString = intercalate " " . map keyFromChar

keyFromChar :: Char -> String
keyFromChar c | isAscii c && isAlphaNum c   = ['<',c,'>']
              | Just s <- lookup c keyNames = concat ["<",s,">"]
              | otherwise                  = error $ "keyFromChar: incomplete table, unexpected '"
                                                       ++ [c] ++ "' (" ++ show (ord c) ++ ")"

-- NOTE: keysymdef.h
keyNames :: [(Char,String)]
keyNames =
    [ ('\'', "apostrophe"),    ('`', "grave"),         ('<', "less"),         ('>', "greater")
    , ('|', "bar"),            ('(', "parenleft"),     (')', "parenright"),   ('[', "bracketleft")
    , (']', "bracketright"),   ('{', "braceleft"),     ('}', "braceright"),   ('+', "plus")
    , ('-', "minus"),          ('^', "asciicircum"),   ('.', "period"),       ('=', "equal")
    , ('~', "asciitilde"),     ('/', "slash"),         ('\\',"backslash"),    ('?', "question")
    , ('!', "exclam"),         ('_', "underscore"),    (':', "colon"),        (';', "semicolon")
    , ('*', "asterisk"),       ('"', "quotedbl"),      ('#', "numbersign"),   (',', "comma")
    , (' ', "space"),          ('\t',"tab"),           ('@', "at"),           ('\n',"Return")
    , ('→', "Right"),          ('←', "Left"),          ('↑', "Up"),           ('↓', "Down")
    , ('&', "ampersand") ]


disamb :: [(String, String)] -> [(String, String)]
disamb table = concatMap f table
  where f e@(k, v) = if null ambs then [e] else [(k ++ " ", v), (k ++ "\t", v)]
          where ambs = [ e2 | e2@(k2, _) <- table,  e /= e2, k `isPrefixOf` k2 ]

