module Md5 exposing (k, s, a, b, c, d)

{-| Constants for the Md5 algorithm
@docs k, s, a, b, c, d
-}

import Array exposing (Array, initialize, fromList)

seedFn : Int -> Int
seedFn n =
  floor (2^32 * abs (sin (1 + toFloat n)))

{-| Constant array
-}
k : Array Int
k = initialize 64 seedFn

{-| Per-round shift amounts
-}
s : Array Int
s = fromList ([7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22 ]
 ++ [5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20 ]
 ++ [4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23 ]
 ++ [6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ])

{-| Initialisation variable A
-}
a : Int
a = 1732584193

{-| Initialisation variable B
-}
b : Int
b = 4023233417

{-| Initialisation variable C
-}
c : Int
c = 2562383102

{-| Initialisation variable D
-}
d : Int
d = 271733878
