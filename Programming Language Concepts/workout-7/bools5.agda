module bools5 where

open import lib

combK : ∀ x y → x imp (y imp x) ≡ tt
combK ff _ = refl
combK tt ff = refl
combK tt tt = refl

ite-not : ∀(A : Set)(x : 𝔹)(y : A)(z : A) → if x then y else z ≡ if ~ x then z else y
ite-not _ tt _ _ = refl
ite-not _ ff _ _ = refl

