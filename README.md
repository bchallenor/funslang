Funslang: A lightweight shading language for graphics processors
================================================================

Funslang is a pure functional shading language for graphics processors. It borrows heavily from ML and Haskell.


Comparison with GLSL
--------------------

Funslang was inspired by my previous implementation of a compiler for OpenGL ES GLSL. GLSL closely resembles C, and many aspects of it are undesirable in a shading language:

### Separation of syntax and semantics
The main function in a GLSL shader is declared to be of type void → void, and output is achieved by writing to magic variables.

### Impurity
Shading cannot cause external side effects, and a shader can always be described as a pure function on numerical data. Most shaders require little or no control flow; for example, loops are statically sized, and conditional branches are rare. Under this assumption, the hardware on which they are executed can be greatly simplified: many shaders can be executed in lockstep by a single execution unit. However, the syntax of GLSL is biased towards control flow, which the shader compiler must undo.

### Complex control flow mechanisms
Many of the architectures to which GLSL will be compiled require control flow to be eliminated. However, GLSL provides the user with complex control flow mechanisms such as `break` and `continue`, which are non-trivial to linearize. The static analysis of even `for` and `while` loops can be difficult.

Unfortunately, these features are rarely used by shaders, and so the effort required to support them is disproportionate. Most loops in shaders are used to apply a function to elements of a vector or array. Here, a functional `fold` or `map` could capture the essence of the operation, while being much easier to analyse.

### Restrictive, hard-coded types
In GLSL, vector types are dimensionally restricted, and the type system is not compositional, so vector types are entirely unrelated to the type of their elements. For example, the floating point vector types are `vec2`, `vec3`, and `vec4`, and unrelated to `float`.

The vector dimension restriction dates back to hardware which had four-component vector arithmetic units. However, vectors are only a syntactic convenience for a modern architecture, which will perform all vector operations with scalar instructions.

### Ad-hoc polymorphism
Because target architectures differ so widely, shaders are often compiled on device. Ad-hoc polymorphism requires a full type checker on device in order to resolve overloads.

Most of the ad-hoc polymorphism in GLSL could actually be parametric if the type system was aware of vectors. For example, the dot product function could be parametric in vector dimension.

### Programming-in-the-large features
Shader programming is programming-in-the-small: shaders are typically a few hundred lines at most, and in a single file. Shading languages do not require the same features as general purpose languages, like GLSL's structs.

### Type annotations
It should be possible to infer most or all of the types in a shader.


Examples
--------

### Vertex shader (`Mandelbrot.vp`)

    % Vertex shader to perform standard transforms
    % and pass coordinates to fragment shader.
    % Author: Ben Challenor

    let getRotX a =
      let s = sin a in
      let c = cos a in
        [ [ 1,  0,  0, 0],
          [ 0,  c, -s, 0],
          [ 0,  s,  c, 0],
          [ 0,  0,  0, 1] ] in

    let getRotY a =
      let s = sin a in
      let c = cos a in
        [ [ c,  0,  s, 0],
          [ 0,  1,  0, 0],
          [-s,  0,  c, 0],
          [ 0,  0,  0, 1] ] in

    let getRotZ a =
      let s = sin a in
      let c = cos a in
        [ [ c, -s,  0, 0],
          [ s,  c,  0, 0],
          [ 0,  0,  1, 0],
          [ 0,  0,  0, 1] ] in

    let lookAt from to up =
      let n = normalize $ from -- to in
      let u = normalize $ up `cross` n in
      let v = n `cross` u in
          [ [u!0, u!1, u!2, -(from `dot` u)],
            [v!0, v!1, v!2, -(from `dot` v)],
            [n!0, n!1, n!2, -(from `dot` n)],
            [ 0 ,  0 ,  0 ,        1       ] ] in


    \ (proj, model, rotx, roty, rotz, from, to, up) ->
    \ () ->
    \ (p) ->

    let view = lookAt from to up in
    let rot = getRotZ rotz # (getRotY roty # getRotX rotx) in

      (proj #. view #. rot #. model #. pad p, (p!![0,1]))


### Fragment shader (`Mandelbrot.fp`)

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


License
-------

Funslang isn't under active development, but I'd love to hear from you if you try it out.

The code is available under the MIT license:

    Copyright (C) 2007-2008 Ben Challenor

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

The thesis is available under the [Creative Commons Attribution-NonCommercial 3.0 Unported License](http://creativecommons.org/licenses/by-nc/3.0/).
