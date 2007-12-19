module Interpreter where

import Representation

a = ValueFun (\(ValueReal dfr1) -> ValueFun (\(ValueReal dfr2) -> ValueReal (DFRealAdd dfr1 dfr2)))