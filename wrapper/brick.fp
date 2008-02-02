\(BrickColor, MortarColor, BrickSize, BrickPct)::(Real 3, Real 3, Real 2, Real 2) ()(LightIntensity, MCposition)::(Real, Real 2).

let [posx, posy] = MCposition // BrickSize in
let pos = map fract [if fract (posy * 0.5) > 0.5 then posx + 0.5 else posx, posy] in
let useBrick = zipWith step pos BrickPct in
let color = zipWith (mix $ useBrick!0 * useBrick!1) MortarColor BrickColor in
  pad $ color **. LightIntensity
