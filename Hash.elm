module Hash exposing (md5, padInput, padInputRec, appendLength, runMd5, toWords)

{-| Hash functions in native elm.

@docs md5, padInput, padInputRec, appendLength, runMd5, toWords

-}

import Md5 exposing (k, s, a, b, c, d)
import Helpers exposing (hexString, leftRot)
import Bitwise exposing (complement, and, or, xor)
import Char exposing (KeyCode)
import String exposing (uncons)
import List exposing (append, map, take)
import Array exposing (Array, length, get, slice, fromList)
import Maybe exposing (withDefault)


type alias Byte =
    Int


{-| Correctly pad input for MD5
-}
padInput : List Byte -> List Byte
padInput input =
    case List.length input % 64 of
        0 ->
            input

        56 ->
            appendLength input (List.length input)

        _ ->
            let
                list =
                    input ++ [ 128 ]

                remaining =
                    55 - (List.length input % 64)
            in
                let
                    newList =
                        padInputRec list remaining
                in
                    appendLength newList (List.length input)


{-| Append the length of the input to itself.

TODO: this only works for lengths < 256. The MD5 spec says that
the 64-bit length of the input should be appended modulo 2^64.
-}
appendLength : List Byte -> Int -> List Byte
appendLength input length =
    input ++ [ 0, 0, 0, 0, 0, 0, 0, length ]


{-| Recursively pad input until it is the correct length
TODO: can probably replace this with List.repeat or something
-}
padInputRec : List Byte -> Int -> List Byte
padInputRec input remaining =
    case remaining of
        0 ->
            input

        _ ->
            let
                list =
                    input ++ [ 0 ]

                remaining' =
                    remaining - 1
            in
                padInputRec list remaining'


type alias Vec =
    ( Int, Int, Int, Int )


type alias Word =
    Int


{-| Take a list of 8-bit bytes and transform them to 32-bit words
-}
toWords : List Byte -> List Word
toWords input =
    let
        inputLen =
            List.length input
    in
        case inputLen of
            0 ->
                []

            _ ->
                case inputLen // 4 of
                    0 ->
                        let
                            paddedBytes =
                                (List.take inputLen input) `List.append` List.repeat (4 - inputLen) 0
                        in
                            [ toWord paddedBytes ]

                    _ ->
                        let
                            bytes =
                                List.take 4 input

                            trailing =
                                toWords (List.drop 4 input)
                        in
                            ([ toWord bytes ]) `List.append` trailing


{-| Given a list of four 8-bit bytes, transform to a 32-bit word
-}
toWord : List Byte -> Word
toWord input =
    input
        |> List.reverse
        |> Array.fromList
        |> Array.indexedMap (\idx -> \num -> 256 ^ idx * num)
        |> Array.foldl (+) 0


{-| Run the MD5 hash algorithm
-}
runMd5 : Array Byte -> Vec
runMd5 message =
    let
        init =
            ( a, b, c, d )
    in
        runRound 0 message init


{-| Run a round of the MD5 hash algorithm
-}
runRound : Int -> Array Word -> Vec -> Vec
runRound roundNo message input =
    let
        ( a, b, c, d ) =
            input

        k' =
            withDefault 0 (get roundNo k)

        s' =
            withDefault 0 (get roundNo s)

        message' =
            slice (0 + (16 * roundNo)) (15 + (16 * roundNo)) message

        lr f g =
            leftRot (a + f + k' + (withDefault 0 (get g message'))) s'
    in
        case roundNo // 16 of
            0 ->
                let
                    f =
                        (b `and` c) `or` ((complement b) `and` d)

                    g =
                        roundNo
                in
                    runRound (roundNo + 1) message ( d, b + (lr f g), b, c )

            1 ->
                let
                    f =
                        ((d `and` b) `or` ((complement d) `and` c))

                    g =
                        ((5 * roundNo) + 1) % 16
                in
                    runRound (roundNo + 1) message ( d, b + (lr f g), b, c )

            2 ->
                let
                    f =
                        (b `Bitwise.xor` c) `Bitwise.xor` d

                    g =
                        ((3 * roundNo) + 5) % 16
                in
                    runRound (roundNo + 1) message ( d, b + (lr f g), b, c )

            3 ->
                let
                    f =
                        c `Bitwise.xor` (b `or` (complement d))

                    g =
                        (7 * roundNo) % 16
                in
                    runRound (roundNo + 1) message ( d, b + (lr f g), b, c )

            _ ->
                input


{-| Turns a string into an md5 hash
  md5 'foo' == 'bar'
-}
md5 : String -> Vec
md5 input =
    input
        |> hexString
        |> padInput
        |> toWords
        |> (take 16)
        |> fromList
        |> runMd5
