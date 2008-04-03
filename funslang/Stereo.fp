% Fragment shader for stereograms.
% Note that the second texture lookup assumes that the pixel it is
% looking up (to the left of this one) has already been rendered.
% Author: Ben Challenor

\ (windowW, tileW, zNear, zFar, numDepthLevels) ->
\ (depthTex, outputTex) ->
\ (windowCoords) ->

% Get depth buffer value.
let [projectedDepth,_,_,_] = sample2D depthTex windowCoords in
% Convert to linear depth so 1 is at camera (not zNear!) and 0 is at zFar.
let zRatio = zNear / zFar in
let linearDepth = 1 - zRatio / (1 - (1 - zRatio) * projectedDepth) in

% Find horizontal shift (in pixels) based on linearDepth.
let shift = linearDepth * numDepthLevels in
% Find horizontal period (in pixels) for this depth.
let period = tileW - shift in

% Find the coordinates with which to index the previous strip.
let prevOutputCoords = [windowCoords!0 - period/windowW, windowCoords!1] in

  sample2D outputTex prevOutputCoords