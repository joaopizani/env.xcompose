module XComposeGen.Letterlike (letterlike) where

import Data.Char (toUpper)
import XComposeGen.Utils (lead, mkPs, Table)


letterlike :: Table
letterlike = concat [greek, nord, sub, super, acc, combining, mathds, mathbb, mathcal, circled]

greek, nord, sub, super, acc, combining, mathds, mathbb, mathcal, circled :: Table


greek = lead '*' $ mkPs (small ++ caps)
  where small = "aα bβ cχ dδ eε fφ gγ hθ iι kκ lλ mμ nη oο pπ qψ rρ sσ tτ vν wω xξ yυ zζ"
        caps  = "AΑ BΒ CΧ DΔ EΕ FΦ GΓ HΘ IΙ KΚ LΛ MΜ NΗ OΟ PΠ QΨ RΡ SΣ TΤ VΝ WΩ XΞ YΥ ZΖ"


nord  = [ ("eth","ð"), ("/o","ø"), ("/O","Ø") ]


sub = lead '_' $ mkPs (subNumMath ++ subLatinSmall ++ subGreek)
  where subNumMath    = "0₀ 1₁ 2₂ 3₃ 4₄ 5₅ 6₆ 7₇ 8₈ 9₉ +₊ -₋ =₌ (₍ )₎"
        subLatinSmall = "aₐ eₑ hₕ iᵢ jⱼ kₖ lₗ mₘ nₙ oₒ pₚ rᵣ sₛ tₜ uᵤ vᵥ xₓ"
        subGreek      = "@ᵦ #ᵧ ^ᵩ"

super = lead '^' $ mkPs (supNumMath ++ supLatinSmall ++ supLatinCapital ++ supGreek)
  where supNumMath      = "0⁰ 1¹ 2² 3³ 4⁴ 5⁵ 6⁶ 7⁷ 8⁸ 9⁹ +⁺ -⁻ =⁼ (⁽ )⁾"
        supLatinSmall   = "aᵃ bᵇ cᶜ dᵈ eᵉ fᶠ gᵍ hʰ iⁱ jʲ kᵏ lˡ mᵐ nⁿ oᵒ pᵖ rʳ sˢ tᵗ uᵘ vᵛ wʷ xˣ yʸ zᶻ"
        supLatinCapital = "Aᴬ Bᴮ Dᴰ Eᴱ Gᴳ Hᴴ Iᴵ Jᴶ Kᴷ Lᴸ Mᴹ Nᴺ Oᴼ Pᴾ Rᴿ Tᵀ Uᵁ Vⱽ Wᵂ"
        supGreek        = "!ᵅ @ᵝ #ᵞ $ᵟ %ᵋ ^ᵠ &ᶿ"


acc = concatMap w [ "aáàâäãå.ȧ",  "b.......ḃ",  "cć.ĉ...çċ",  "d.......ḋ",  "eéèêë...ė",  "f.......ḟ"
                  , "g.......ġ",  "iíìîïĩ...",  "h.......ḣ",  "m.......ṁ",  "nń...ñ..ṅ",  "oóòôöõ..ȯ"
                  , "p.......ṗ",  "r.......ṙ",  "s.......ṡ",  "t.......ṫ",  "uúùûü....",  "w.......ẇ"
                  , "x.......ẋ",  "yý.ŷÿ...ẏ",  "z.......ż" ]
  where w (k : cs) = concat $ zipWith z "'`i;~o,." cs
          where z p c = if c == '.' then []
                                   else [  ([p,k], [c]),  ([p, toUpper k], [toUpper c])  ]

combining = lead '0' $ mkPs "'́ `̀ î ;̈ ~̃ o̊ ,̧ .̇"


mathds = lead 'b' $ mkPs (small ++ caps ++ other)
  where small = "a𝕒 b𝕓 c𝕔 d𝕕 e𝕖 f𝕗 g𝕘 h𝕙 i𝕚 j𝕛 k𝕜 l𝕝 m𝕞 n𝕟 o𝕠 p𝕡 q𝕢 r𝕣 s𝕤 t𝕥 u𝕦 v𝕧 w𝕨 x𝕩 y𝕪 z𝕫"
        caps  = "A𝔸 B𝔹 Cℂ Dⅅ E𝔼 F𝔽 G𝔾 Hℍ I𝕀 J𝕁 K𝕂 L𝕃 M𝕄 Nℕ O𝕆 Pℙ Qℚ Rℝ S𝕊 T𝕋 U𝕌 V𝕍 W𝕎 X𝕏 Y𝕐 Zℤ"
        other = "0𝟘 1𝟙 2𝟚 3𝟛 4𝟜 5𝟝 6𝟞 7𝟟 8𝟠 9𝟡 #ℾ *ℿ (⅀"

mathbb = lead 'B' $ mkPs (small ++ caps)
  where small = "a𝐚 b𝐛 c𝐜 d𝐝 e𝐞 f𝐟 g𝐠 h𝐡 i𝐢 j𝐣 k𝐤 l𝐥 m𝐦 n𝐧 o𝐨 p𝐩 q𝐪 r𝐫 s𝐬 t𝐭 u𝐮 v𝐯 w𝐰 x𝐱 y𝐲 z𝐳"
        caps  = "A𝐀 B𝐁 C𝐂 D𝐃 E𝐄 F𝐅 G𝐆 H𝐇 I𝐈 J𝐉 K𝐊 L𝐋 M𝐌 N𝐍 O𝐎 P𝐏 Q𝐐 R𝐑 S𝐒 T𝐓 U𝐔 V𝐕 W𝐖 X𝐗 Y𝐘 Z𝐙"

mathcal = lead 'C' $ mkPs (small ++ caps)
  where small = "a𝒶 b𝒷 c𝒸 d𝒹 eℯ f𝒻 gℊ h𝒽 i𝒾 j𝒿 k𝓀 l𝓁 m𝓂 n𝓃 oℴ p𝓅 q𝓆 r𝓇 s𝓈 t𝓉 u𝓊 v𝓋 w𝓌 x𝓍 y𝓎 z𝓏"
        caps  = "A𝒜 Bℬ C𝒞 D𝒟 Eℰ Fℱ G𝒢 Hℋ Iℐ J𝒥 K𝒦 Lℒ Mℳ N𝒩 O𝒪 P𝒫 Q𝒬 Rℛ S𝒮 T𝒯 U𝒰 V𝒱 W𝒲 X𝒳 Y𝒴 Z𝒵"

circled = special ++ (lead '(' $ mkPs $ mathNum ++ capital ++ small)
  where mathNum = "+⊕ -⊖ ^⊗ /⊘ *⊛ =⊜ .⊙ 0⓪ 1① 2② 3③ 4④ 5⑤ 6⑥ 7⑦ 8⑧ 9⑨"
        capital = "AⒶ BⒷ CⒸ DⒹ EⒺ FⒻ GⒼ HⒽ IⒾ JⒿ KⓀ LⓁ MⓂ NⓃ OⓄ PⓅ QⓆ RⓇ SⓈ TⓉ UⓊ VⓋ WⓌ XⓍ YⓎ ZⓏ"
        small   = "aⓐ bⓑ cⓒ dⓓ eⓔ fⓕ gⓖ hⓗ iⓘ jⓙ kⓚ lⓛ mⓜ nⓝ oⓞ pⓟ qⓠ rⓡ sⓢ tⓣ uⓤ vⓥ wⓦ xⓧ yⓨ zⓩ"
        special = lead 'o' $ mkPs "^° b● w○ :◌ O◯"
