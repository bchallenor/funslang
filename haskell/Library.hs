module Library where


data Op
  = Op1Prefix' !Op1Prefix
  | Op1Postfix' !Op1Postfix
  | Op2Infix' !Op2Infix
--  | Op3Prefix' !Op3Prefix

  deriving (Show, Eq)


data Op1Prefix
  = Op1Neg -- i/f -> i/f; i/f n -> i/f n; i/f m n -> i/f m n
  | Op1Not -- bool -> bool
--   --
--   | Op1_sum -- i/f n -> i/f
--   | Op1_product -- i/f n -> i/f
--   | Op1_any -- bool n -> bool
--   | Op1_all -- bool n -> bool
--   | Op1_min -- i/f n -> i/f
--   | Op1_max -- i/f n -> i/f
--   --
--   | Op1_sample -- t1d -> [f 1 -> f 4]; t2d -> [f 2 -> f 4]; t3d -> [f 3 -> f 4]; tcube -> [f 3 -> f 4]
--   --
--   | Op1_len -- f n -> f
--   | Op1_dot -- f n -> [f n -> f]
--   | Op1_normalize -- f n -> f n
--   | Op1_faceforward -- f n -> [f n -> f n -> f n]
--   | Op1_reflect -- f n -> [f n -> f n]
--   | Op1_refract -- f n -> [f n -> f -> f n]

  deriving (Show, Eq)


data Op1Postfix
  = Op1Transpose -- i/f m n -> i/f n m
  
  deriving (Show, Eq)


data Op2Infix
  = Op2Subscript -- a n -> Int -> a
  | Op2Swizzle -- a n -> Int m -> a m
  | Op2Append -- a n -> a m -> a m+n
  | Op2Mul -- i/f -> i/f -> i/f; i/f n -> i/f n -> i/f n; i/f m n -> i/f m n -> i/f m n
  | Op2Div -- as mul
  | Op2LinearMul -- i/f q p -> i/f r q -> i/f r p; i/f q -> i/f r q -> i/f r; i/f q p -> i/f q -> i/f p
  | Op2ScaleMul -- i/f n -> i/f -> i/f n
  | Op2ScaleDiv -- as scale div
  | Op2Add -- as mul
  | Op2Sub -- as mul
  | Op2LessThan -- i/f -> i/f -> bool
  | Op2LessThanEqual -- as less than
  | Op2GreaterThan -- as less than
  | Op2GreaterThanEqual -- as less than
  | Op2Equal -- notfunction a => a -> a -> bool
  | Op2NotEqual -- as equal
  | Op2And -- bool -> bool -> bool
  | Op2Or -- bool -> bool -> bool
  
  deriving (Show, Eq)


-- data Op3Prefix
--   = Op3_map -- (a -> b) -> a n -> b n
--   | Op3_foldl -- (a -> b -> a) -> a -> b n -> a
--   | Op3_foldr -- (a -> b -> b) -> b -> a n -> b
--   
--   deriving (Show, Eq)


-- data LibFun
--   : Fun_sin -- f -> f
--   | Fun_cos -- f -> f
--   | Fun_tan -- f -> f
--   | Fun_asin -- f -> f
--   | Fun_acos -- f -> f
--   | Fun_atan -- f -> f -> f
--   | Fun_pow -- f -> f -> f
--   | Fun_exp -- f -> f
--   | Fun_exp2 -- f -> f
--   | Fun_log -- f -> f
--   | Fun_log2 -- f -> f
--   | Fun_sqrt -- f -> f
--   | Fun_rcpsqrt -- f -> f
--   | Fun_rcp -- f -> f
--   | Fun_abs -- f -> f
--   | Fun_sign -- f -> f
--   | Fun_floor -- f -> f
--   | Fun_ceil -- f -> f
--   | Fun_round -- f -> f
--   | Fun_truncate -- f -> f
--   | Fun_fract -- f -> f
--   | Fun_mod -- f -> f -> f
--   | Fun_clamp -- f -> f -> f -> f
--   | Fun_mix -- f -> f -> f -> f
--   | Fun_step -- f -> f -> f
--   | Fun_smoothstep -- f -> f -> f -> f
--   --
--   | Fun_toInt -- f -> i
--   | Fun_toFloat -- i -> f
--   --
--   | Fun_cross -- f 3 -> f 3 -> f 3

--   deriving (Show, Eq)
