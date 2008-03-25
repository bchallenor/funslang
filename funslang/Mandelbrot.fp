% Fragment shader for drawing Mandelbrot sets
% Author: Ben Challenor
% Based on Julia.frag (GLSL) by 3Dlabs

let max_iterations = 50 in

\ (zoom, centerX, centerY, innerColor, outerColor1, outerColor2) ->
\ () ->
\ ([posX, posY]) ->

% starting values
let cReal = posX * zoom + centerX in
let cImag = posY * zoom + centerY in

% iteration function
let f (r, i, iter) =
  let rnew = r * r - i * i + cReal in
  let inew = 2 * r * i + cImag in
  let l2 = rnew * rnew + inew * inew in
    if l2 < 4 then (rnew, inew, iter+1) else (r, i, iter) in

% iterate
let (rnew, inew, iter) = unroll f max_iterations (0, 0, 0) in

let basecolor =
  if iter == max_iterations
    then innerColor
    else zipWith (mix (iter / max_iterations)) outerColor1 outerColor2  in

  pad basecolor
