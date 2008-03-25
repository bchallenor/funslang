% Fragment shader for drawing Julia sets
% Author: Ben Challenor
% Based on Julia.frag (GLSL) by 3Dlabs

let MAX_ITERATIONS = 50 in

\ (Zoom, Xcenter, Ycenter, InnerColor, OuterColor1, OuterColor2, Creal, Cimag) ->
\ () ->
\ ([Xpos, Ypos]) ->

% starting values
let r = Xpos * Zoom + Xcenter in
let i = Ypos * Zoom + Ycenter in

% iteration function
let f (r, i, iter) =
  let rnew = r * r - i * i + Creal in
  let inew = 2 * r * i + Cimag in
  let l2 = rnew * rnew + inew * inew in
    if l2 < 4 then (rnew, inew, iter+1) else (r, i, iter) in

% iterate
let (rnew, inew, iter) = unroll f MAX_ITERATIONS (r, i, 0) in

let basecolor =
  if iter == MAX_ITERATIONS
    then InnerColor
    else zipWith (mix (iter / MAX_ITERATIONS)) OuterColor1 OuterColor2  in

  pad basecolor