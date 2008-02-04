% Fragment shader for drawing Mandelbrot sets
% Author: Ben Challenor
% Based on Julia.frag (GLSL) by 3Dlabs

let MAX_ITERATIONS = 50 in

\ (Zoom, Xcenter, Ycenter, InnerColor, OuterColor1, OuterColor2) .
\ () .
\ ([Xpos, Ypos], LightIntensity) .

% starting values
let Creal = Xpos * Zoom + Xcenter in
let Cimag = Ypos * Zoom + Ycenter in

% iteration function
let f (r, i, iter) =
  let rnew = r * r - i * i + Creal in
  let inew = 2 * r * i + Cimag in
  let l2 = rnew * rnew + inew * inew in
    if l2 < 4 then (rnew, inew, iter+1) else (r, i, iter) in

% iterate
let (rnew, inew, iter) = unroll f MAX_ITERATIONS (0, 0, 0) in

let basecolor =
  if iter == MAX_ITERATIONS
    then InnerColor
    else zipWith (mix (iter / MAX_ITERATIONS)) OuterColor1 OuterColor2  in

let litcolor = basecolor **. LightIntensity in

  pad litcolor
