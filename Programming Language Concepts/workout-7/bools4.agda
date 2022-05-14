module bools4 where

open import lib

----------------------------------------------------------------------
-- some additional problems
----------------------------------------------------------------------

&&-distrib : ∀ x y z → x && (y || z) ≡ (x && y) || (x && z)
&&-distrib tt tt _ = refl
&&-distrib tt _ tt = refl
&&-distrib tt ff ff = refl
&&-distrib ff _ _ = refl


