-- Fragment shader for drawing Julia sets
-- Author: Ben Challenor
-- Based on Julia.frag (GLSL) by 3Dlabs

\ (
Zoom :: Float,
Xcenter :: Float,
Ycenter :: Float,
InnerColor :: Float 3,
OuterColor1 :: Float 3,
OuterColor2 :: Float 3,
Creal :: Float,
Cimag :: Float
) .

\ (
Position :: Float 3,
LightIntensity :: Float
) .

-- iteration function
let iterate (r, i, iter)::(Float, Float, Int) _::Int =
  let rnew = r * r - i * i + Creal in
  let inew = 2.0 * r * i + Cimag in
  let l2 = rnew * rnew + inew * inew in
    if l2 < 4.0 then (rnew, inew, iter+1) else (r, i, iter) in

-- starting values
let r = Position!0 * Zoom + Xcenter in
let i = Position!1 * Zoom + Ycenter in

-- tweak max iterations here
let (r, i, iter) = foldl iterate (r, i, 0) [1..50] in

-- more efficient to recalculate l2 rather than threading it out of the iteration
let l2 = r * r + i * i in

let basecolor =
  if (l2 < 4.0)
    then InnerColor
    else mix OuterColor1 OuterColor2 (fract (iter * 0.05)) in

let litcolor = basecolor * LightIntensity in

let clampedcolor = clamp litcolor 0.0 1.0 in

  clampedcolor @ [1.0]
