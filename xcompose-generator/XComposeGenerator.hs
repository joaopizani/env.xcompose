{-# LANGUAGE PatternGuards #-}
import NP.Unicode
import Data.Char (isAscii, isAlphaNum, ord)
import Data.List (intercalate)
import Control.Monad (liftM)
import System.Environment (getArgs)


main :: IO ()
main = liftM head getArgs >>= flip writeFile (bindings ++ footer)
    where footer = ""

bindings :: String
bindings = unlines . map genLine $ disamb $ concat [greek, symbols, subscripts, superscripts]

genLine :: (String, String) -> String
genLine (name, target) = "<Multi_key> " ++ keysFromString name ++ " : \"" ++ target ++ "\""
    where keysFromString = intercalate " " . map keyFromChar

keyFromChar :: Char -> String
keyFromChar c | isAscii c && isAlphaNum c   = ['<',c,'>']
              | Just s <- lookup c keyNames = concat ["<",s,">"]
              | otherwise = error $ "keyFromChar: incomplete table, unexpected '"
                                        ++ [c] ++ "' (" ++ show (ord c) ++ ")"

-- NOTE: /usr/include/X11/keysymdef.h
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
