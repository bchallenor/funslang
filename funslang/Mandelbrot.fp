% Fragment shader for drawing Mandelbrot sets
% Author: Ben Challenor
% Based on Mandel.frag (GLSL) by 3Dlabs

let maxIterations = 50 in

\ (zoom, center, innerColor, outerColor1, outerColor2) ->
\ () ->
\ (pos) ->

% starting values
let [cr, ci] = pos **. zoom ++ center in

% iteration function
let f ([r, i], iter) =
  let r' = r * r - i * i + cr in
  let i' = 2 * r * i + ci in
  let len2 = r' * r' + i' * i' in
    if len2 < 4.0 then ([r', i'], iter+1) else ([r, i], iter) in

% iterate
let ([r, i], iter) = unroll f maxIterations ([0.0, 0.0], 0) in

let basecolor =
  if iter == maxIterations
    then innerColor
    else zipWith (mix (fract (iter * 0.05))) outerColor1 outerColor2  in

  pad basecolor
