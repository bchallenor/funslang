module Library where


data Operator1
  = Op1Transpose
  | Op1Neg
  | Op1Not
  
  deriving (Show, Eq)


data Operator2
  = Op2Subscript -- t n -> (Int -> t)
  | Op2Swizzle -- t n -> Int m -> (t m)
  | Op2Append -- t n -> t m -> (t m+n)
  | Op2Mul -- i/f -> i/f -> (i/f); i/f n -> i/f n -> (i/f n); i/f m n -> i/f m n -> (i/f m n)
  | Op2Div
  | Op2LinearMul -- i/f q p -> i/f r q -> i/f r p; i/f q -> i/f r q -> i/f r; i/f q p -> i/f q -> i/f p
  | Op2ScaleMul -- i/f n -> (i/f -> i/f n)
  | Op2ScaleDiv
  | Op2Add
  | Op2Sub
  | Op2LessThan -- i/f -> i/f -> b
  | Op2LessThanEqual
  | Op2GreaterThan
  | Op2GreaterThanEqual
  | Op2Equal -- t -> t -> b (t not function)
  | Op2NotEqual
  | Op2Identical
  | Op2NotIdentical
  | Op2And -- b -> b -> b
  | Op2Or -- b -> b -> b
  
  deriving (Show, Eq)
