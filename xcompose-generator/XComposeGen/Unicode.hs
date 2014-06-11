module XComposeGen.Unicode (greek, symbols, subscripts, superscripts, checkAmbs, disamb) where

import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Control.Applicative



greek :: [(String, String)]
greek = [ (name, unicode)          | (_,          name, unicode) <- greekData] ++
        [ ([lead, short], unicode) | (Just short, _,    unicode) <- greekData , lead <- "*"]

-- | Triples: (shorthand, name, unicode)
greekData :: [(Maybe Char, String, String)]
greekData =
    [ (Just 'a' , "alpha"   , "α"),   (Just 'b' , "beta"    , "β"),   (Just 'g' , "gamma"   , "γ")
    , (Just 'G' , "Gamma"   , "Γ"),   (Just 'd' , "delta"   , "δ"),   (Just 'D' , "Delta"   , "Δ")
    , (Just 'e' , "epsilon" , "ε"),   (Just 'z' , "zeta"    , "ζ"),   (Just 'N' , "eta"     , "η")
    , (Just 'E' , "eta"     , "η"),   (Nothing  , "theta"   , "θ"),   (Nothing  , "Theta"   , "Θ")
    , (Just 'i' , "iota"    , "ι"),   (Just 'k' , "kapa"    , "κ"),   (Just 'l' , "lambda"  , "λ")
    , (Just 'L' , "Lambda"  , "Λ"),   (Just 'm' , "mu"      , "μ"),   (Just 'n' , "nu"      , "ν")
    , (Just 'x' , "xi"      , "ξ"),   (Just 'o' , "omicron" , "ο"),   (Just 'p' , "pi"      , "π")
    , (Just 'P' , "Pi"      , "Π"),   (Just 'r' , "rho"     , "ρ"),   (Just 's' , "sigma"   , "σ")
    , (Just 'S' , "Sigma"   , "Σ"),   (Just 't' , "tau"     , "τ"),   (Just 'f' , "phi"     , "φ")
    , (Just 'F' , "Phi"     , "Φ"),   (Just 'c' , "chi"     , "χ"),   (Just 'C' , "Chi"     , "Χ")
    , (Nothing  , "psi"     , "ψ"),   (Nothing  , "Psi"     , "Ψ"),   (Just 'w' , "omega"   , "ω")
    , (Just 'O' , "Omega"   , "Ω") ]



symbols :: [(String, String)]
symbols = concat [custom, pars, accents, mathbb, mathcal, circled, simple]


custom, pars, accents, mathbb, mathcal, circled, simple :: [(String,String)]

custom = [ ("jp", "joaopizani"),  ("jp@", "joaopizani@gmail.com") ]

pars = concat [map fst parPairs, map snd parPairs, map both parPairs]
    where
        both ((x, y), (_, z)) = ('s' : x, y ++ z)
        parPairs =
            [ p "<"  "⟨"    ">"  "⟩",     p "<<"  "⟪"   ">>"  "⟫",     p "|("   "〖"    ")|"   "〗"
            , p "{|" "⦃"    "|}" "⦄",     p "{{"  "⦃"   "}}"  "⦄",     p "[["   "⟦"    "]]"   "⟧"
            , p "|_" "⌊"    "_|" "⌋",     p "r|_" "⌈"   "r_|" "⌉",     p "rs|_" "⌜"    "rs_|" "⌝" ]
            where p x y z t = ((x, y), (z, t))

accents = concat
  [ f 'a' ('á','à','â','ä','ã','å',' ','ȧ'),     f 'b' (' ',' ',' ',' ',' ',' ',' ','ḃ')
  , f 'c' ('ć',' ','ĉ',' ',' ',' ','ç','ċ'),     f 'd' (' ',' ',' ',' ',' ',' ',' ','ḋ')
  , f 'e' ('é','è','ê','ë',' ',' ',' ','ė'),     f 'f' (' ',' ',' ',' ',' ',' ',' ','ḟ')
  , f 'g' (' ',' ',' ',' ',' ',' ',' ','ġ'),     f 'i' ('í','ì','î','ï','ĩ',' ',' ',' ')
  , f 'h' (' ',' ',' ',' ',' ',' ',' ','ḣ'),     f 'm' (' ',' ',' ',' ',' ',' ',' ','ṁ')
  , f 'n' ('ń',' ',' ',' ','ñ',' ',' ','ṅ'),     f 'o' ('ó','ò','ô','ö','õ',' ',' ','ȯ')
  , f 'p' (' ',' ',' ',' ',' ',' ',' ','ṗ'),     f 'r' (' ',' ',' ',' ',' ',' ',' ','ṙ')
  , f 's' (' ',' ',' ',' ',' ',' ',' ','ṡ'),     f 't' (' ',' ',' ',' ',' ',' ',' ','ṫ')
  , f 'u' ('ú','ù','û','ü',' ',' ',' ',' '),     f 'w' (' ',' ',' ',' ',' ',' ',' ','ẇ')
  , f 'x' (' ',' ',' ',' ',' ',' ',' ','ẋ'),     f 'y' ('ý',' ','ŷ','ÿ',' ',' ',' ','ẏ')
  , f 'z' (' ',' ',' ',' ',' ',' ',' ','ż') ]
  where
    f key (cute, grav, circ, diar, tild, ring, ced, dot) =
      concat [b 'e' cute, b '`' grav, b 'i' circ, b 'u' diar, b '~' tild, b 'o' ring, b ',' ced, b '.' dot]
      where
        b p ac = if ac == ' ' then [] else [ ([p, key], [ac]),  ([p, toUpper key], [toUpper ac]) ]


mathbb = [ (lead : l, [u]) | lead <- ['|','b'], (l,u) <- mathbb' ]
    where mathbb' = [ ("E",'𝔼'),  ("N",'ℕ'),   ("H",'ℍ'),   ("P",'ℙ'),   ("R",'ℝ'),   ("C",'ℂ')
                    , ("D",'ⅅ'),  ("Q",'ℚ'),   ("Z",'ℤ'),   ("0",'𝟘'),   ("1",'𝟙'),   ("2",'𝟚')
                    , ("3",'𝟛'),  ("4",'𝟜'),   ("5",'𝟝'),   ("6",'𝟞'),   ("7",'𝟟'),   ("8",'𝟠')
                    , ("9",'𝟡'),  ("gg",'ℽ'),  ("gG",'ℾ'),  ("gP",'ℿ'),  ("gS",'⅀') ]

mathcal =
    [ ("cP","℘"), ("cL","ℒ"), ("cR","ℛ"), ("cN","𝒩"), ("cE","ℰ"), ("cF","ℱ"), ("cH","ℋ")
    , ("cI","ℐ"), ("cM","ℳ"), ("ce","ℯ"), ("cg","ℊ"), ("co","ℴ"), ("cl","ℓ") ]


circled =
    [ ("oo","°"), ("o^","°"), ("ob","●"), ("op","∙"), ("ow","○"), ("ov","⎉"), ("o..","◌"), ("oO","◯")
    , c "+"  "⊕",   c "-"  "⊖",   c "x"  "⊗",   c "/"  "⊘",   c "*"  "⊛",   c "="  "⊜",   c "."  "⊙"
    , c "()" "⊚",   c "0"  "⓪",   c "1"  "①",   c "2"  "②",   c "3"  "③",   c "4"  "④",   c "5"  "⑤"
    , c "6"  "⑥",   c "7"  "⑦",   c "8"  "⑧",   c "9"  "⑨",   c "10" "⑩",   c "11" "⑪",   c "12" "⑫"
    , c "13" "⑬",   c "14" "⑭",   c "15" "⑮",   c "16" "⑯",   c "17" "⑰",   c "18" "⑱",   c "19" "⑲"
    , c "20" "⑳",   c "A"  "Ⓐ",   c "B"  "Ⓑ",   c "C"  "Ⓒ",   c "D"  "Ⓓ",   c "E"  "Ⓔ",   c "F"  "Ⓕ"
    , c "G"  "Ⓖ",   c "H"  "Ⓗ",   c "I"  "Ⓘ",   c "J"  "Ⓙ",   c "K"  "Ⓚ",   c "L"  "Ⓛ",   c "M"  "Ⓜ"
    , c "N"  "Ⓝ",   c "O"  "Ⓞ",   c "P"  "Ⓟ",   c "Q"  "Ⓠ",   c "R"  "Ⓡ",   c "S"  "Ⓢ",   c "T"  "Ⓣ"
    , c "U"  "Ⓤ",   c "V"  "Ⓥ",   c "W"  "Ⓦ",   c "X"  "Ⓧ",   c "Y"  "Ⓨ",   c "Z"  "Ⓩ",   c "a"  "ⓐ"
    , c "b"  "ⓑ",   c "c"  "ⓒ",   c "d"  "ⓓ",   c "e"  "ⓔ",   c "f"  "ⓕ",   c "g"  "ⓖ",   c "h"  "ⓗ"
    , c "i"  "ⓘ",   c "j"  "ⓙ",   c "k"  "ⓚ",   c "l"  "ⓛ",   c "m"  "ⓜ",   c "n"  "ⓝ",   c "o"  "ⓞ"
    , c "p"  "ⓟ",   c "q"  "ⓠ",   c "r"  "ⓡ",   c "s"  "ⓢ",   c "t"  "ⓣ",   c "u"  "ⓤ",   c "v"  "ⓥ"
    , c "w"  "ⓦ",   c "x"  "ⓧ",   c "y"  "ⓨ",   c "z"  "ⓩ" ]
    where c x y = ("(" ++ x ++ ")", y)


simple = concat [quant, op, arrows, rels, eqs, misc, curr, turns, squar, punct, dash, quots, nord]

quant, op, arrows, rels, eqs, misc, curr, turns, squar, punct, dash, quots, nord :: [(String,String)]

quant =
    [ ("FA", "∀"),   ("EX", "∃"),   ("rA", "∀"),   ("rE", "∃"),   ("/rE", "∄"),  ("r/E", "∄")
    , ("na", "∇"),   ("rgD", "∇"),  ("r'D", "∇"),  ("sum", "∑") ]

op =
    [ ("<|", "◁"),  ("|>", "▷"),   ("b|>", "▸"),   ("><", "⋈"),   ("<)", "◅"),   ("(>", "▻")
    , ("v", "∨"),   ("u", "∪"),    ("n", "∩"),     ("V", "⋁"),    ("+-", "±"),   ("+u", "⊎")
    , ("u+", "⊎"),  ("u[]", "⊔"),  ("n[]", "⊓"),   ("^", "∧"),    ("/\\", "∧"),  ("\\/", "∨")
    , ("o", "∘"),   (".", "·"),    ("...", "…"),   ("c...", "⋯"), ("v...", "⋮"), ("x","×")
    , ("neg", "¬"), ("-.", "∸"),   ("-:", "÷"),    ("sqrt", "√"), ("cbrt", "∛"), ("<w>", "◇")
    , ("<b>", "◈") ]

arrows =
    [ ("<-","←"),   ("->","→"),   ("|->","↦"),  ("<-|","↤"),   ("<--","⟵"),  ("-->","⟶")
    , ("|-->","⟼"), ("o->","⇴"),  ("|^","↑"),   ("|v","↓"),    ("|vv","↡"),  ("|^^","↟")
    , ("||^","⇑"),  ("||v","⇓"),  ("==>","⟹"),  ("=>","⇒"),    ("<=","⇐"),   ("<=>","⇔")
    , ("|=>","⇨"),  ("<=|","⇦"),  ("~>","↝"),   ("<~","↜"),    ("~->","⇝"),  ("<-~","⇜")
    , ("<-<","↢"),  (">->","↣"),  ("<->","↔"),  ("<|-|>","⇿"), ("-|>","⇾"),  ("|<-","⇤")
    , ("->|","⇥"),  (">>=","↠"),  ("->>","↠"),  ("/-","↼"),    ("\\-","↽"),  ("-/","⇁")
    , ("-\\","⇀"),  ("-|->","⇸"), ("c->","↪"),  ("rc->","↩"),  ("/v","↯"),   ("u^","↺")
    , ("->->","⇉"), ("2->","⇉"),  ("<-<-","⇇"), ("-><-","⇄"),  ("<-->","⇆"), ("3->","⇶") ]

rels =
    [ ("c=","⊆"),    ("/c=","⊈"),   ("c","⊂"),     ("/c","⊄"),     ("c-","∈"),     ("/c-","∉")
    , ("c/=","⊊"),   ("rc=","⊇"),   ("rc","⊃"),    ("rc-","∋"),    ("r/c-","∌"),   ("rc/=","⊋")
    , (">=","≥"),    ("=<","≤"),    ("/>=","≱"),   ("/=<","≰"),    ("c[]","⊏"),    ("rc[]","⊐")
    , ("c[]=","⊑"),  ("rc[]=","⊒"), ("/c[]=","⋢"), ("/rc[]=","⋣"), ("c[]/=","⋤"),  ("rc[]/=","⋥") ]

eqs =
    [ ("=def","≝"),   ("=?","≟"),    ("=o","≗"),    ("==","≡"),    ("===","≣"),  ("~~","≈")
    , ("/~~","≉"),    ("/~=","≇"),   ("~~-","≊"),   ("~~~","≋"),   ("~-","≃"),   ("~=","≅")
    , ("~","∼"),      ("/=","≠"),    ("/==","≢"),   (":=","≔"),    ("=:","≕") ]

misc =
    [ ("Bot","⊥"),  ("Top","⊤"),  ("||","∥"),   ("l","ℓ"),    ("99","«"),   ("00","»"),  ("90","«»")
    , ("  "," "),   (" ."," "),   ("::","∷"),   (":","∶"),    ("r;","⁏"),   ("0","∅"),   ("r8","∞")
    , ("*","★"),    ("/'l","ƛ"),  ("d","∂"),    ("#b","♭"),   ("#f","♮"),   ("##","♯"),  ("Hot","♨")
    , ("Cut","✂"),  ("Pen","✎"),  ("Tick","✓"), ("dag","†"),  ("mic","µ"),  ("os","§"),  ("so","§")
    , ("PP","¶"),   ("CCCP","☭"), ("ck","⌥"),   (":)","☺"),   (":(","☹"),   ("<3","♥"),  ("sp","♠")
    , ("Sp","♠"),   ("di","♦"),   ("Di","♦"),   ("<>","♦"),   ("Cl","♣"),   ("d1","⚀"),  ("d2","⚁")
    , ("d3","⚂"),   ("d4","⚃"),   ("d5","⚄"),   ("d6","⚅"),   ("tm","™"),   ("Ro","✊"),  ("Pa","✋")
    , ("Sc","✌"),   ("rt","ʇ"),   ("rh","ɥ"),   ("re","ǝ"),   ("ra","ɐ"),   ("oe","œ"),  ("OE","Œ")
    , ("ae","æ"),   ("AE","Æ") ]

curr  = [ ("B|","฿"), ("e=","€"), ("L-","£"), ("Y=","¥"), ("x.","¤") ]
turns = [ ("|-","⊢"), ("|/-","⊬"), ("-|","⊣"), ("|=","⊨"), ("|/=","⊭"), ("||-","⊩"), ("|||-","⊪") ]
squar = [ ("[+]","⊞"), ("[-]","⊟"), ("[x]","⊠"), ("[.]","⊡"), ("[]","∎"), ("[ ]","☐"), ("[T]","☑") ]
punct = [ ("!!","‼"), ("??","⁇"), ("?!","⁈"), ("?b!","‽"), ("!?","⁉"), ("r?", "¿"), ("r!", "¡") ]
dash  = [ ("-","−") ]
quots = [ ("\"","“”"), ("r`","′") ]
nord  = [ ("eth","ð"), ("/o","ø"), ("/O","Ø") ]




zipscripts :: Char -> String -> String -> [(String, String)]
zipscripts c ascii unicode = zip (fmap ((c:) . pure) ascii) (fmap pure unicode)

-- "ₕₖₗₘₙₚₛₜ" http://hackage.haskell.org/trac/ghc/ticket/5519
subscripts :: [(String, String)]
subscripts = zipscripts '_' ("0123456789+-=()aeioruvx"++"hklmnpst")
                            ("₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑᵢₒᵣᵤᵥₓ"++ hklmnpst )
    where hklmnpst = "\8341\8342\8343\8344\8345\8346\8347\8348"

-- NOTE that qCFQSVXYZ are missing
superscripts :: [(String, String)]
superscripts = zipscripts '^'
  "0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUW"
  "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂ"



checkAmbs :: [(String, String)] -> [(String, String)]
checkAmbs table = check
    where ambs = [ (x, y) | v@(x, _) <- table, w@(y, _) <- table,  v /= w, x `isPrefixOf` y ]
          check | null ambs = table
                | otherwise = error $ "checkAmbs: ambiguous declarations for " ++ show ambs


disamb :: [(String, String)] -> [(String, String)]
disamb table = concatMap f table
    where f e@(k, v) = if null ambs then [e] else [(k ++ " ", v), (k ++ "\t", v)]
              where ambs = [ e2 | e2@(k2, _) <- table,  e /= e2, k `isPrefixOf` k2 ]



{-
   ∣  ⋆  ⁺   ⁻  •  ✶  ≺  ″  ≳  ≲  ◁  ∗  ≰  ‿  ⊴  ≮  □  ⇛   ⊸  ≯
   ↛  ⋐  ⁆  ⁅  ϕ  ◂  ≴  ≁  ⑵  ⑴   ̂  ≻  ►  ∔  ▶  ≛  ⦈  ⦇  ⑶
   ⋃   ⋂   ≵   ½  ’ —  ∁  Μ  ı

CE "₠" # EURO-CURRENCY SIGN
C/ "₡" # COLON SIGN
/C "₡" # COLON SIGN
Cr "₢" # CRUZEIRO SIGN
Fr "₣" # FRENCH FRANC SIGN
L= "₤" # LIRA SIGN
=L "₤" # LIRA SIGN
m/ "₥" # MILL SIGN
/m "₥" # MILL SIGN
N= "₦" # NAIRA SIGN
=N "₦" # NAIRA SIGN
Pt "₧" # PESETA SIGN
Rs "₨" # RUPEE SIGN
W= "₩" # WON SIGN
=W "₩" # WON SIGN
   "₪" # NEW SHEQEL SIGN
d- "₫" # DONG SIGN
   "₭" # KIP SIGN
   "₮" # TUGRIK SIGN
   "₯" # DRACHMA SIGN
   "₰" # GERMAN PENNY SIGN
   "₱" # PESO SIGN
   "₲" # GUARANI SIGN
   "₳" # AUSTRAL SIGN
   "₴" # HRYVNIA SIGN
   "₵" # CEDI SIGN
|c "¢" # CENT SIGN
c| "¢" # CENT SIGN
c/ "¢" # CENT SIGN
/c "¢" # CENT SIGN
-}

