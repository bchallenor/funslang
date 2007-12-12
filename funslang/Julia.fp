let mix x y a =
  x *. (1.0 - a) + y *. a in

let fract x::Num = x in


-- Fragment shader for drawing Julia sets
-- Author: Ben Challenor
-- Based on Julia.frag (GLSL) by 3Dlabs

let MAX_ITERATIONS = 50 in

\ (Zoom, Xcenter, Ycenter, InnerColor, OuterColor1, OuterColor2, Creal, Cimag) .

\ (Position, LightIntensity) .

-- iteration function
let f (r, i, iter) =
  let rnew = r * r - i * i + Creal in
  let inew = 2 * r * i + Cimag in
  let l2 = rnew * rnew + inew * inew in
    if l2 < 4 then (rnew, inew, iter+1) else (r, i, iter) in

-- starting values
let r = Position!0 * Zoom + Xcenter in
let i = Position!1 * Zoom + Ycenter in

-- iterate
let (r, i, iter) = unroll f MAX_ITERATIONS (r, i, 0) in

-- more efficient to recalculate l2 rather than threading it out of the iteration
let l2 = r * r + i * i in

let basecolor =
  if (l2 < 4)
    then InnerColor
    else mix OuterColor1 OuterColor2 (fract (iter * 0.05)) in

let litcolor = basecolor **. LightIntensity in

  litcolor
