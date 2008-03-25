\(BrickColor, MortarColor, BrickSize, BrickPct)()(LightIntensity, MCposition) ->

let [posx, posy] = MCposition // BrickSize in
let pos = map fract [if fract (posy * 0.5) > 0.5 then posx + 0.5 else posx, posy] in
let useBrick = zipWith step pos BrickPct in
let color = zipWith (mix $ useBrick!0 * useBrick!1) MortarColor BrickColor in
  pad $ color **. LightIntensity
