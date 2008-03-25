\(brickColor, mortarColor, brickSize, brickPct)()(lightIntensity, mcPosition) ->

let [posX, posY] = mcPosition // brickSize in
let pos = map fract [if fract (posY * 0.5) > 0.5 then posX + 0.5 else posX, posY] in
let useBrick = zipWith step pos brickPct in
let color = zipWith (mix $ useBrick!0 * useBrick!1) mortarColor brickColor in
  pad $ color **. lightIntensity
