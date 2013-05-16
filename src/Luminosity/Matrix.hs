-- Matrix operation module

module Luminosity.Matrix
(
  Matrix
, munit
, mzero
, det
, getRow
, getCol
, (%+%)
, (%-%)
, (*%)
, (%*%)
) where

import Luminosity.Vector

type Matrix = VectorT Vector

-- | Identity matrix
munit :: Matrix
munit = Vector unitX unitY unitZ

-- | Zero matrix
mzero :: Matrix
mzero = Vector vzero vzero vzero

-- | Calculate matrix determinant
det :: Matrix -> Scalar
det (Vector (Vector a11 a12 a13)
            (Vector a21 a22 a23)
            (Vector a31 a32 a33)) = a11 * a22 * a33 + a13 * a21 * a32 + a12 * a23 * a31
                                  - a13 * a22 * a31 - a12 * a21 * a33 - a11 * a23 * a32

-- | Matrix row getter
getRow :: Int -> Matrix -> Vector
getRow 1 (Vector x _ _) = x
getRow 2 (Vector _ y _) = y
getRow 3 (Vector _ _ z) = z
getRow _ _ = error "Wrong row number!"

-- | Matrix column getter
getCol :: Int -> Matrix -> Vector
getCol 1 (Vector (Vector a _ _) (Vector b _ _) (Vector c _ _)) = Vector a b c
getCol 2 (Vector (Vector _ a _) (Vector _ b _) (Vector _ c _)) = Vector a b c
getCol 3 (Vector (Vector _ _ a) (Vector _ _ b) (Vector _ _ c)) = Vector a b c
getCol _ _ = error "Wrong column number!"

-- Operator fixities
infixl 6 %+%, %-%
infixl 7 *%
infixl 8 %*%

-- | Matrix summation
(%+%) :: Matrix -> Matrix -> Matrix
(Vector x1 y1 z1) %+% (Vector x2 y2 z2) = Vector
    (x1 <+> x2)
    (y1 <+> y2)
    (z1 <+> z2)

-- | Matrix subtraction
(%-%) :: Matrix -> Matrix -> Matrix
(Vector x1 y1 z1) %-% (Vector x2 y2 z2) = Vector
    (x1 <-> x2)
    (y1 <-> y2)
    (z1 <-> z2)

-- | Scalar by matrix multiplication
(*%) :: Scalar -> Matrix -> Matrix
a *% (Vector x y z) = Vector
    (a *> x)
    (a *> y)
    (a *> z)

-- | Matrix multiplication
(%*%) :: Matrix -> Matrix -> Matrix
m1 %*% m2 = Vector
    (Vector ((getRow 1 m1) <.> (getCol 1 m2))
            ((getRow 1 m1) <.> (getCol 2 m2))
            ((getRow 1 m1) <.> (getCol 3 m2)))
    (Vector ((getRow 2 m1) <.> (getCol 1 m2))
            ((getRow 2 m1) <.> (getCol 2 m2))
            ((getRow 2 m1) <.> (getCol 3 m2)))
    (Vector ((getRow 3 m1) <.> (getCol 1 m2))
            ((getRow 3 m1) <.> (getCol 2 m2))
            ((getRow 3 m1) <.> (getCol 3 m2)))
