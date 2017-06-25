module Csv.Decode exposing
    ( Csv
    , Decoder
    , Errors(..)
    , decode
    , decodeCsv
    , next, field
    , (</>)
    , oneOf
    , map
    , maybe
    )

{-|

CSV decoding, heavily inspired by [url-parser][].

Note this library does not include an underlying CSV parser. It assumes you 
are using something like [periodic/elm-csv][periodic] or 
[lovasoa/elm-csv][lovasoa] to get from `String` to `Csv`, where 
`Csv` is:

    type alias Csv =
      { headers : List String
      , records : List (List String)
      }

This library gets you the rest of the way to a list of your own types. It also
collects errors together with the index of the record they occurred on, see 
below.

Using periodic/elm-csv (which returns a `Result (List String) Csv`):

    Csv.parse rawData |> Csv.Decode.decode myDecoder

Using lovasoa.elm-csv (which returns a plain `Csv`):

    Csv.parse rawData |> Csv.Decode.decodeCsv myDecoder

You can define decoders based on field position or on header name. See 
examples below.


# Types
@docs Csv, Decoder, Errors

# Running
@docs decode, decodeCsv, next, field

# Combining
@docs (</>), oneOf, map, maybe


[url-parser]: https://github.com/evancz/url-parser

[periodic]: https://github.com/periodic/elm-csv

[lovasoa]: https://github.com/lovasoa/elm-csv

-}

import Tuple

{-|
The raw CSV data structure.

-}
type alias Csv = 
  { headers : List String
  , records : List (List String)
  }

{-|
A value that encapsulates how to decode CSV records (`List String`)

-}
type Decoder a b = 
    Decoder (State a -> Result String (State b))

type alias State value =
    { visited : List (String,String)
    , unvisited : List (String,String)
    , value : value
    }

{-|
Errors can either be
  
  1. Errors in the underlying CSV parsing (`CsvErrors`), or
  2. Errors in decoding a list of parsed records to models (`DecodeErrors`)

Note that the latter reports the record index together with the error message.

-}
type Errors
   = CsvErrors (List String)
   | DecodeErrors (List (Int, String))


{-|
Decode the raw result of CSV parsing. 

Typically you chain them together like this (using periodic/elm-csv):

    Csv.parse rawData |> Csv.Decode.decode myDecoder

-}
decode : Decoder (a -> a) a -> Result (List String) Csv -> Result Errors (List a)
decode decoder =
    Result.mapError CsvErrors >> Result.andThen (decodeCsv decoder)

{-|
Decode raw CSV data. 

This is useful if you already have a `Csv` structure not
wrapped in a `Result`, for instance lovasoa/elm-csv parses CSV strings this way.

-}

decodeCsv : Decoder (a -> a) a -> Csv -> Result Errors (List a)
decodeCsv decoder {headers, records} =
    List.map (decodeRecord decoder headers) records
        |> sequenceResultsAccumErrs
        |> Result.mapError DecodeErrors

decodeRecord : Decoder (a -> a) a -> List String -> List String -> Result String a
decodeRecord (Decoder decoder) headers record =
    (Result.map .value) <| decoder <| 
        { visited = []
        , unvisited = List.map2 (,) headers record 
        , value = identity
        } 


{-|
Decode the next field from the input: positional decoding.

Use this when you are certain of the order of the fields. It is faster than 
header-based decoding.

    type alias Coordinates = 
        { x : Float, y : Float, z : Float }

    decodeCoordinates : Decoder (Coordinates -> a) a
    decodeCoordinates =
        map Coordinates 
            ( next String.toFloat </> 
            , next String.toFloat </>
            , next String.toFloat
            )
-}
next : (String -> Result String a) -> Decoder (a -> b) b
next fn =
    Decoder <| \{visited,unvisited,value} ->
        case unvisited of
            [] ->
                Err "Past the end of the record"
            
            (rawField,rawValue) :: rest ->
                case fn rawValue of
                    Ok nextValue ->
                        Ok <| State ((rawField,rawValue) :: visited) rest (value nextValue)
                    Err msg ->
                        Err msg


{-|
Decode the named field from the input: header-based decoding.

Use this when you do not want to rely on the order of the fields.

    type alias Nutrition = 
       { name : String, calories : Int, protein : Float }

    decodeNutrition : Decoder (Nutrition -> a) a
    decodeNutrition =
        map Nutrition 
            ( field "name" Ok </> 
            , field "calories"  String.toInt </>
            , field "protein" String.toFloat
            )

Note that position- and header-based decoding can be combined, but it is not
generally recommended.

-}
field : String -> (String -> Result String a) -> Decoder (a -> b) b
field name fn =
    Decoder <| \{visited,unvisited,value} ->
        case (listFind (\(name_,_) -> name_ == name) unvisited) of
            Nothing ->
                Err ("No field named '" ++ name ++ "' found")

            Just (rawField,rawValue) ->
                case fn rawValue of
                    Ok nextValue ->
                        Ok <| State visited unvisited (value nextValue)
                    Err msg ->
                        Err msg

{-|
Decode multiple fields.

-}
(</>) : Decoder a b -> Decoder b c -> Decoder a c
(</>) (Decoder decodeBefore) (Decoder decodeAfter) =
    Decoder <| \state -> 
        Result.andThen decodeAfter (decodeBefore state)

infixr 7 </>


{-|
Try a bunch of different decoders, using the first one that succeeds.

    type IntOrFloat
       = Int_ Int
       | Float_ Float

    decode : Decoder (IntOrFloat -> a) a
    decode =
        oneOf 
          [ next (String.toInt >> Result.map Int_)
          , next (String.toFloat >> Result.map Float_)
          ]

-}
oneOf : List (Decoder a b) -> Decoder a b
oneOf decoders =
    Decoder <| \state ->
        ( listFindOk (\(Decoder p) -> p state) decoders
            |> Maybe.withDefault (Err "No decoders succeeded")
        )

{-|
Transform a decoder. 

Typically used to feed a bunch of parsed state into a type constructor.

-}
map : a -> Decoder a b -> Decoder (b -> c) c
map subValue (Decoder decoder) =
    Decoder <| \{visited,unvisited,value} ->
         Result.map (mapHelp value) <| decoder <|
             { visited = visited
             , unvisited = unvisited
             , value = subValue
             }

mapHelp : (a -> b) -> State a -> State b
mapHelp fn {visited,unvisited,value} =
    { visited = visited
    , unvisited = unvisited
    , value = fn value
    }



{-|
A convenience function for converting empty strings to `Nothing`.

-}
maybe : (String -> Result String a) -> (String -> Result String (Maybe a))
maybe fn =
    \s -> if s == "" then (Ok Nothing) else (fn s |> Result.map Just)


{- Internal utils
-}

sequenceResultsAccumErrs : List (Result e a) -> Result (List (Int,e)) (List a)
sequenceResultsAccumErrs list =
    let
        accum next (i,result) =
            case (next, result) of
                (Ok b, Ok a) ->
                    (i - 1, Ok (b :: a))
                (Err b, Ok a) ->
                    (i - 1, Err [(i,b)])
                (Ok b, Err a) ->
                    (i - 1, Err a)
                (Err b, Err a) ->
                    (i - 1, Err ((i,b) :: a))
    in
        List.foldr accum ((List.length list - 1), Ok []) list |> Tuple.second


-- Same as List.Extra.find
listFind : (a -> Bool) -> List a -> Maybe a
listFind pred list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if pred first then
                Just first
            else
                listFind pred rest


listFindOk : (a -> Result e b) -> List a -> Maybe (Result e b)
listFindOk fn list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case fn first of
                Ok b ->
                    Just <| Ok b
                Err e ->
                    listFindOk fn rest


