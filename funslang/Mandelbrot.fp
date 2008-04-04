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
let f ([r, i], [r2, i2], iter) =
  let r' = r2 - i2 + cr in
  let i' = 2 * r * i + ci in
  let r2' = r' * r' in
  let i2' = i' * i' in
    (r2' + i2' < 4.0, ([r', i'], [r2', i2'], iter+1)) in

% iterate
let (_, _, iter) = iterate f maxIterations ([0.0, 0.0], [0.0, 0.0], 0) in

% color
let basecolor =
  if iter == maxIterations
    then innerColor
    else zipWith (mix (fract (iter * 0.05))) outerColor1 outerColor2  in

  pad basecolor
