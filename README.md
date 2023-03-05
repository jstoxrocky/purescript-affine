# purescript-transformation-matrix

Matrix operations for 4x4 affine transformation matrices used in 3D computer graphics. 

## About

Inspired by [threejs](https://github.com/mrdoob/three.js).

## Example

```purescript
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