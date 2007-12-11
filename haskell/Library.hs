module Library where


data Operator1
  = Op1Transpose
  | Op1Neg
  | Op1Not
  
  deriving (Show, Eq)


data Operator2
  = Op2Subscript
  | Op2Swizzle
  | Op2Append
  | Op2Mul
  | Op2Div
  | Op2LinearMul
  | Op2ScaleMul
  | Op2ScaleDiv
  | Op2Add
  | Op2Sub
  | Op2LessThan
  | Op2LessThanEqual
  | Op2GreaterThan
  | Op2GreaterThanEqual
  | Op2Equal
  | Op2NotEqual
  | Op2Identical
  | Op2NotIdentical
  | Op2And
  | Op2Or
  
  deriving (Show, Eq)


