module XComposeGen.Symbols (symbols) where

import XComposeGen.Utils (lead, mkPs, Table)


symbols :: Table
symbols = concat [custom, pars, quant, op, arrows, rels, eqs, misc, curr, turns, squar, punct, dash]

custom, pars, quant, op, arrows, rels, eqs, misc, curr, turns, squar, punct, dash :: Table


custom = [ ("@u", "joaopizani"),  ("@m", "joaopizani@gmail.com") ]

pars = map fst ps ++ map snd ps
  where p x y z t = ((x, y), (z, t))
        ps = [ p "<" "⟨" ">" "⟩",   p "<<" "⟪" ">>" "⟫", p "|(" "〖" ")|" "〗",  p "{|" "⦃" "|}" "⦄"
             , p "[[" "⟦" "]]" "⟧", p "|_" "⌊" "_|" "⌋", p "r|_" "⌈" "r_|" "⌉" , p "r^|_" "⌜" "r^_|" "⌝" ]



quant = [ ("FA", "∀"),  ("EX", "∃"),  ("/EX", "∄"),  ("na", "∇"),  ("sum", "∑") ]

op = [ ("<|", "◁"), ("|>", "▷"), ("b|>", "▸"), ("><", "⋈"), ("<)", "◅"), ("(>", "▻"), ("v", "∨")
     , ("u", "∪"), ("n", "∩"), ("V", "⋁"), ("+-", "±"), ("+u", "⊎"), ("u+", "⊎"), ("u[]", "⊔")
     , ("n[]", "⊓"), ("^", "∧"), ("o", "∘"), (".", "·"), ("...", "…"), ("c...", "⋯")
     , ("v...", "⋮"), ("x","×"), ("neg", "¬"), ("-.", "∸"), ("-:", "÷"), ("sqrt", "√")
     , ("cbrt", "∛"), ("<w>", "◇"), ("<o>", "◈") ]

arrows = [ ("<-","←"), ("->","→"), ("|->","↦"), ("<-|","↤"), ("<--","⟵"), ("-->","⟶"), ("|-->","⟼")
         , ("|^","↑"), ("|v","↓"), ("|vv","↡"), ("|^^","↟"), ("||^","⇑"), ("||v","⇓")
         , ("==>","⟹"), ("=>","⇒"), ("<=","⇐"), ("<=>","⇔"), ("|=>","⇨"), ("<=|","⇦"), ("~>","↝")
         , ("<~","↜"), ("~->","⇝"), ("<-~","⇜"), ("<-<","↢"), (">->","↣"), ("<->","↔"), ("<|-|>","⇿")
         , ("-|>","⇾"), ("|<-","⇤"), ("->|","⇥"), (">>=","↠"), ("->>","↠"), ("/-","↼"), ("\\-","↽")
         , ("-/","⇁"), ("-\\","⇀"), ("-|->","⇸"), ("/v","↯"), ("u^","↺")
         , ("->->","⇉"), ("2->","⇉"), ("<-<-","⇇"), ("-><-","⇄"), ("<-->","⇆"), ("3->","⇶")
         , ("\\/","⤨"), ("/\\","⤪") ]

rels = [ ("c=","⊆"), ("/c=","⊈"), ("c","⊂"), ("/c","⊄"), ("c-","∈"), ("/c-","∉")
       , ("rc=","⊇"), ("rc","⊃"), ("rc-","∋"), ("r/c-","∌"), (">=","≥"), ("=<","≤")
       , ("/>=","≱"), ("/=<","≰"), ("c[]","⊏"), ("rc[]","⊐"), ("c[]=","⊑"), ("rc[]=","⊒")
       , ("/c[]=","⋢"), ("/rc[]=","⋣") ]

eqs = [ ("=d","≝"), ("=?","≟"), ("=o","≗"), ("==","≡"), ("===","≣"), ("~~","≈"), ("/~~","≉")
      , ("/~=","≇"), ("~~-","≊"), ("~~~","≋"), ("~-","≃"), ("~=","≅"), ("~","∼"), ("/=","≠")
      , ("/==","≢"), (":=","≔") ]

misc = [ ("Bot","⊥"), ("Top","⊤"), ("||","∥"), ("l","ℓ"), ("99","«"), ("00","»"), ("90","«»")
       , ("  "," "), ("::","∷"), (":","∶"), ("r;","⁏"), ("0","∅"), ("r8","∞"), ("*","★")
       , ("/'l","ƛ"), ("d","∂"), ("#b","♭"), ("#f","♮"), ("##","♯"), ("Hot","♨"), ("Cut","✂"), ("Pen","✎")
       , ("Tic","✓"), ("dag","†"), ("mic","µ"), ("so","§"), ("PP","¶"), ("CCCP","☭")
       , ("ck","⌥"), (":)","☺"), (":(","☹"), ("<3","♥"), ("<33","💗"), ("sp","♠"), ("Sp","♠"), ("di","♦"), ("Di","♦")
       , ("<b>","♦"), ("Cl","♣"), ("d1","⚀"), ("d2","⚁"), ("d3","⚂"), ("d4","⚃"), ("d5","⚄"), ("d6","⚅")
       , ("tm","™"), ("Ro","✊"), ("Pa","✋"), ("Sc","✌"), ("rt","ʇ"), ("rh","ɥ"), ("re","ǝ"), ("ra","ɐ")
       , ("oe","œ"), ("OE","Œ"), ("ae","æ"), ("AE","Æ") ]

curr  = [ ("B|","฿"), ("e=","€"), ("L-","£"), ("Y=","¥"), ("x.","¤") ]
turns = [ ("|-","⊢"), ("|/-","⊬"), ("-|","⊣"), ("|=","⊨"), ("|/=","⊭"), ("||-","⊩"), ("|||-","⊪") ]
squar = lead '[' $ mkPs "+⊞ -⊟ x⊠ .⊡ q∎ w□ b■ T☑"
punct = [ ("!!","‼"), ("??","⁇"), ("?!","⁈"), ("?b!","‽"), ("!?","⁉"), ("r?","¿"), ("r!","¡"), ("''","′"), ("\"\"","″") ]
dash  = [ ("-n","–"), ("-m","—") ]




{-
∣ ⋆ • ✶ ≺ ≳ ≲ ◁ ∗ ≰ ‿ ⊴ ≮ □ ⇛ ⊸ ≯ ↛ ⋐ ⁆ ⁅ ◂ ≴ ≁ ⑵ ⑴ ≻ ► ∔ ▶ ≛ ⦈ ⦇ ⑶ ⋃ ⋂ ≵ ½
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
|c "¢" # CENT SIGN
c/ "¢" # CENT SIGN
/c "¢" # CENT SIGN
-}
