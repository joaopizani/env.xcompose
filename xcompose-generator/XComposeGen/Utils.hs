module XComposeGen.Utils where

import Data.Char (isAscii, isAlphaNum, ord)
import Data.List (isPrefixOf)


type Table = [(String, String)]

lead :: Char -> Table -> Table
lead ld ps = [ (ld : k, v) | (k, v) <- ps ]

mkPs :: String -> Table
mkPs = mkPs' . filter (/= ' ')
  where mkPs' s = let (kv, r) = splitAt 2 s  in  case kv of
                                                   []      -> []
                                                   (k : v) -> ([k], v) : mkPs' r


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
    , ('&', "ampersand"),      ('$', "dollar"),        ('%', "percent")  ]

disamb :: Table -> Table
disamb table = concatMap f table
  where f e@(k, v) = if null ambs then [e] else [(k ++ " ", v), (k ++ "\t", v)]
          where ambs = [ e2 | e2@(k2, _) <- table,  e /= e2, k `isPrefixOf` k2 ]

