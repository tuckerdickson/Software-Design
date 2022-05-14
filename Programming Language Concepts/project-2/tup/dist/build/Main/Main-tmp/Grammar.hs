{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module Grammar where
import Tokens
import Tup
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn t4 t5 t6 t7 t8 t9 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: t5 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: t6 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: t7 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyInTok :: (Token) -> (HappyAbsSyn t4 t5 t6 t7 t8 t9)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4 t5 t6 t7 t8 t9) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x86\x31\x00\xc3\x18\x80\x61\x0c\x00\x00\x00\x00\xe1\x00\x00\x00\x00\x10\xc2\x00\x00\x00\x00\x00\x00\x00\x42\x18\x00\x00\x00\x00\x00\x00\x40\xe8\x03\x80\x01\x00\x10\x42\x00\x08\x61\x00\x00\x00\x00\x00\x00\x00\x90\x03\x00\x00\x00\x40\x08\x03\x20\x84\x01\x10\xc2\x00\x08\x61\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x00\x00\x00\x10\xc2\x00\x00\x00\x00\x40\x00\x00\x42\x18\x00\x00\x00\x80\x10\x02\x00\x00\x00\x00\x00\x00\x10\xc2\x00\x08\x61\x00\x00\x00\x00\x08\x07\x00\x80\x03\x80\x10\x02\x80\xe4\x00\x00\x00\x00\x10\xc2\x00\x90\x00\x00\x84\x30\x00\x00\x07\x00\x00\x00\x80\x10\x02\x80\xe0\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTup","Program","Line","Equation","Expression","Patterns","Pattern","newline","'('","')'","'='","'|'","','","'#'","'!'","'+'","'-'","'*'","var","int","%eof"]
        bit_start = st Prelude.* 23
        bit_end = (st Prelude.+ 1) Prelude.* 23
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..22]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x01\x00\x01\x00\x01\x00\x00\x00\x69\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x08\x00\x00\x00\xf6\xff\x20\x00\x13\x00\xff\xff\x41\x00\x14\x00\x2c\x00\x77\x00\x48\x00\x4a\x00\x4a\x00\x4a\x00\x4a\x00\x00\x00\x61\x00\x61\x00\x61\x00\x61\x00\x51\x00\x58\x00\x2c\x00\x0a\x00\x38\x00\x00\x00\xff\xff\x00\x00\x00\x00\x58\x00\x58\x00\x61\x00\x71\x00\x61\x00\xff\xff\x5d\x00\x00\x00\x58\x00\x16\x00\x58\x00\x61\x00\x00\x00\xff\xff\x66\x00\x00\x00\x25\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x23\x00\x75\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x57\x00\x02\x00\x00\x00\x3f\x00\x00\x00\x3f\x00\x46\x00\x4d\x00\x4f\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x5f\x00\x6b\x00\x00\x00\x6b\x00\x00\x00\x36\x00\x00\x00\x00\x00\x76\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x00\x00\x00\x00\x83\x00\x00\x00\x84\x00\x00\x00\x00\x00\x85\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfd\xff\x00\x00\xfd\xff\xfc\xff\x00\x00\xfb\xff\x00\x00\xed\xff\xfa\xff\xeb\xff\xee\xff\x00\x00\x00\x00\x00\x00\xeb\xff\x00\x00\xe7\xff\xe8\xff\x00\x00\xef\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xf0\xff\xf1\xff\xf2\xff\xf9\xff\xef\xff\x00\x00\xf6\xff\x00\x00\xef\xff\xec\xff\x00\x00\xe7\xff\xe8\xff\x00\x00\x00\x00\xf5\xff\x00\x00\xf8\xff\x00\x00\x00\x00\xf4\xff\x00\x00\x00\x00\x00\x00\xf7\xff\xe9\xff\x00\x00\x00\x00\xf3\xff\x00\x00\xea\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x02\x00\x0e\x00\x03\x00\x07\x00\x05\x00\x07\x00\x08\x00\x02\x00\x0c\x00\x04\x00\x0c\x00\x0d\x00\x07\x00\x06\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x02\x00\x04\x00\x05\x00\x03\x00\x06\x00\x07\x00\x06\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x02\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x03\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x02\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x03\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x02\x00\x05\x00\x02\x00\xff\xff\x06\x00\x07\x00\x03\x00\x07\x00\x03\x00\x02\x00\x0c\x00\x0d\x00\x0c\x00\x0d\x00\x07\x00\x03\x00\x02\x00\xff\xff\x02\x00\x0c\x00\x0d\x00\x07\x00\x03\x00\x07\x00\x03\x00\x02\x00\x0c\x00\x0d\x00\x0c\x00\x0d\x00\x07\x00\x03\x00\x02\x00\x04\x00\x05\x00\x0c\x00\x0d\x00\x07\x00\x03\x00\x03\x00\x03\x00\x06\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x03\x00\x09\x00\x0a\x00\x0b\x00\x04\x00\x03\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x04\x00\x01\x00\x02\x00\x03\x00\x03\x00\x09\x00\x0a\x00\x0b\x00\x06\x00\x03\x00\x05\x00\x09\x00\x0a\x00\x0b\x00\x03\x00\x04\x00\x05\x00\x03\x00\x03\x00\xff\xff\xff\xff\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x24\x00\x06\x00\x07\x00\xff\xff\x12\x00\x25\x00\x20\x00\x08\x00\x09\x00\x10\x00\x26\x00\xeb\xff\x0a\x00\x0b\x00\x11\x00\x2c\x00\xef\xff\xef\xff\xef\xff\x12\x00\x0b\x00\xe7\xff\x27\x00\x28\x00\x33\x00\xe7\xff\xe7\xff\x34\x00\xed\xff\xed\xff\xed\xff\xe7\xff\xed\xff\x07\x00\x0b\x00\x02\x00\x03\x00\x04\x00\x08\x00\x38\x00\x16\x00\x17\x00\x18\x00\x14\x00\x0b\x00\x07\x00\x18\x00\x02\x00\x03\x00\x04\x00\x08\x00\x12\x00\xef\xff\xef\xff\xef\xff\x20\x00\x0b\x00\x07\x00\x20\x00\x07\x00\x00\x00\xe8\xff\x08\x00\x28\x00\x08\x00\x0c\x00\x10\x00\x1e\x00\x0b\x00\x14\x00\x0b\x00\x11\x00\x1c\x00\x07\x00\x00\x00\x07\x00\x22\x00\x0b\x00\x08\x00\x1b\x00\x08\x00\x1a\x00\x07\x00\x1e\x00\x0b\x00\x14\x00\x0b\x00\x08\x00\x19\x00\x07\x00\x22\x00\x0e\x00\x20\x00\x0b\x00\x08\x00\x2e\x00\x0c\x00\x2c\x00\x2f\x00\x14\x00\x0b\x00\x16\x00\x17\x00\x18\x00\x36\x00\x16\x00\x17\x00\x18\x00\x15\x00\x0c\x00\x16\x00\x17\x00\x18\x00\x16\x00\x17\x00\x18\x00\x31\x00\x02\x00\x03\x00\x04\x00\x2a\x00\x16\x00\x17\x00\x18\x00\x1f\x00\x29\x00\x2f\x00\x16\x00\x17\x00\x18\x00\x0c\x00\x0d\x00\x0e\x00\x34\x00\x31\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 24) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24)
	]

happy_n_terms = 15 :: Prelude.Int
happy_n_nonterms = 6 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_2  0# happyReduction_1
happyReduction_1 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 ([happy_var_1] ++ happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_0  0# happyReduction_2
happyReduction_2  =  happyIn4
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (Equ happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  happyIn5
		 (Newline
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  happyIn5
		 (Exclam
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happySpecReduce_3  1# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (Test happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happyReduce 4# 2# happyReduction_7
happyReduction_7 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenVar happy_var_1) -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn6
		 (WithoutGuard happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happyReduce 6# 2# happyReduction_8
happyReduction_8 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenVar happy_var_1) -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut7 happy_x_6 of { happy_var_6 -> 
	happyIn6
		 (WithGuard happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_3  3# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenVar happy_var_1) -> 
	case happyOutTok happy_x_2 of { (TokenVar happy_var_2) -> 
	case happyOutTok happy_x_3 of { (TokenVar happy_var_3) -> 
	happyIn7
		 (FunCall1 happy_var_1 happy_var_2 happy_var_3
	)}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_3  3# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenVar happy_var_1) -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (FunCall2 happy_var_1 happy_var_2 happy_var_3
	)}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happyReduce 5# 3# happyReduction_11
happyReduction_11 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn7
		 (ExpressionTuple happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happyReduce 7# 3# happyReduction_12
happyReduction_12 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut7 happy_x_6 of { happy_var_6 -> 
	happyIn7
		 (ExpressionTriple happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_3  3# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (Add happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_3  3# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (Sub happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_3  3# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (Mult happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_1  3# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenVar happy_var_1) -> 
	happyIn7
		 (ExpressionVar happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_1  3# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenInt happy_var_1) -> 
	happyIn7
		 (ExpressionInt happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_1  3# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn7
		 (ExpressionNull
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_2  4# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn8
		 ([happy_var_1] ++ happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_0  4# happyReduction_20
happyReduction_20  =  happyIn8
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happyReduce 7# 5# happyReduction_21
happyReduction_21 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	case happyOut9 happy_x_6 of { happy_var_6 -> 
	happyIn9
		 (PatternTriple happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happyReduce 5# 5# happyReduction_22
happyReduction_22 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	happyIn9
		 (PatternTuple happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happySpecReduce_1  5# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenVar happy_var_1) -> 
	happyIn9
		 (PatternVar happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_1  5# happyReduction_24
happyReduction_24 happy_x_1
	 =  happyIn9
		 (PatternNull
	)

happyNewToken action sts stk [] =
	happyDoAction 14# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenNewline happy_dollar_dollar -> cont 1#;
	TokenLParen -> cont 2#;
	TokenRParen -> cont 3#;
	TokenEqual -> cont 4#;
	TokenPipe -> cont 5#;
	TokenComma -> cont 6#;
	TokenPound -> cont 7#;
	TokenExclam -> cont 8#;
	TokenPlus -> cont 9#;
	TokenMinus -> cont 10#;
	TokenTimes -> cont 11#;
	TokenVar happy_dollar_dollar -> cont 12#;
	TokenInt happy_dollar_dollar -> cont 13#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 14# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseTup tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {x' = happyOut4 x} in x'))

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
