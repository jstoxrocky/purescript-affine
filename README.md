# purescript-transformation-matrix

[Spatial matrix transformations](https://www.brainvoyager.com/bv/doc/UsersGuide/CoordsAndTransforms/SpatialTransformationMatrices.html) for 4x4 affine matrices used in 3D graphics. 

## Purpose

My goal is to build a pure functional set of libraries to perform the heavy lifting of creating, transforming, and interacting with 3D objects. The results of these operations can then be passed to a lightweight WebGL renderer ([threejs](https://github.com/mrdoob/three.js) or other) that is used only for display purposes. The `purescript-transformation-matrix` library will be the linear algebraic component of this set of libraries. The manipulation of geometries, materials, and meshes will be a separate library. 

## Why not just use three.js?

While this project is heavily inspired by [threejs](https://github.com/mrdoob/three.js) I have found my self frustrated with [threejs](https://github.com/mrdoob/three.js) for several reasons. Here are a few of those reasons:

1. Linear alegbraic operations are scattered across class inheritance structures making it difficult to find and reason about.
2. Pure mathematical logic is tightly coupled with [threejs](https://github.com/mrdoob/three.js) specific object logic.  
3. Vectors and matrices are modified in place making it hard to keep track of modifications to their values.
4. Source code lacks types.

I believe that investing in a homegrown, pure functional, 3D graphics library is worth the time and effort and hope that you think so too! Code contributions are always welcome :)

## Advantages

In the context of [threejs](https://github.com/mrdoob/three.js), this library:
1. Provides a direct API for linear algebraic operations used in 3D graphics
2. Does not tighly couple linear alebraic operations with higher order 3D graphic data types
3. Is not a FFI wrapper over [threejs](https://github.com/mrdoob/three.js) [[1]](https://gitlab.com/theunixman/purescript/purescript-threejs/-/tree/0.1-dev) [[2]](https://github.com/anthonyquizon/purescript-three) [[3]](https://github.com/RazZziel/purescript-threejs)
   
In the context of existing Purescript matrix libraries, this library: 
1. Is specific to the matrix operations and dimensions used in 3D graphics (spatial/affine transformations, 4x4 matrices, 3x1 vectors)
2. Is not a general purpose matrix algebra library [[1]](https://github.com/csicar/purescript-sized-matrices) [[2]](https://github.com/kritzcreek/purescript-matrices/blob/master/src/Matrix.purs)
3. Is runtime safe and does not use unsafe functions [[1]](https://github.com/mgmeier/purescript-matrix/blob/322e496ccec220b329442c82564019bed0af484c/src/Data/Matrix.purs#L110) [[2]](https://github.com/csicar/purescript-sized-matrices/blob/7331f4088124a64383d4ecf755f5b2e06c5150d3/src/Data/Matrix.purs#L45)
4. Is not reliant on the user to make sure that their matrix functions are [passed matrices of compatible dimensions](https://github.com/klangner/purescript-linear-algebra/blob/7f7e943dbaf726a43007e9c895a92232b20aa9db/src/LinearAlgebra/Matrix.purs#L169)






## Examples

### Translate

```purescript
import Data.TransformationMatrix.Matrix4 
    ( Matrix4(..)
    , translate )

matrix = Matrix4
    1.0 0.0 0.0 1.0
    0.0 1.0 0.0 2.0
    0.0 0.0 1.0 3.0
    0.0 0.0 0.0 1.0

translation = Vector3 2.0 3.0 4.0

result = translate translation matrix
-- result == Matrix4
--   1.0 0.0 0.0 3.0
--   0.0 1.0 0.0 5.0
--   0.0 0.0 1.0 7.0
--   0.0 0.0 0.0 1.0
```

### Scale

```purescript
import Data.TransformationMatrix.Matrix4 
    ( Matrix4(..)
    , scale )

matrix = Matrix4
    1.0 0.0 0.0 1.0
    0.0 1.0 0.0 2.0
    0.0 0.0 1.0 3.0
    0.0 0.0 0.0 1.0

multiplier = 2.0

result = scale multiplier matrix
-- result == Matrix4
--   2.0 0.0 0.0 1.0
--   0.0 2.0 0.0 2.0
--   0.0 0.0 2.0 3.0
--   0.0 0.0 0.0 1.0
```