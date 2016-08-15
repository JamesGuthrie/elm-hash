module Helpers exposing (leftRot, hexString, hex)

import Char exposing (KeyCode, toCode, fromCode)
import Array exposing (Array)
import String exposing (uncons)
import Bitwise exposing (shiftLeft, shiftRight, or)
import List exposing (append)

type alias Byte = Int

{-| Turns a Char into a List Byte
  hex 'a' == [99]
  hex 'Ԑ' == [5, 160]
-}
hex : Char -> List Byte
hex input =
  let
      code =
        toCode input
  in
    case code // 256 of
      0 -> [code]
      _ -> hex (fromCode (code // 256)) `append` [code % 256]


{-| Turn a String into a List of Bytes
-}
hexString : String -> List Byte
hexString input =
    case uncons input of
        Just ( x, xs ) ->
            hex x `append` hexString xs

        Nothing ->
            []

{-| Leftrotate x by c
This is defined in the MD5 specification.
-}
leftRot : Int -> Int -> Int
leftRot x c =
  (x `shiftLeft` c) `or` (x `shiftRight` (32 - c))
