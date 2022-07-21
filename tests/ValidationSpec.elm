module ValidationSpec exposing (..)

import Expect
import Dict
import Test exposing (Test, describe, test)

import Validation exposing
  ( Validation(..), extract, pure, map
  , map2, map3, map4, map5, map6, map7
  , mapError, andThen, withValidation
  , fromMaybe, toMaybe, applyOrCollect
  )

type alias ValidatedPerson =
  { firstName : String
  , lastName : String
  , age : Int
  , email : String
  , ipAddress : String
  }

suite : Test
suite =
  describe "Validation" <|
    [ describe "pure"
        [ test "Wrap in Validation data constructor" <|
          \_ ->
            let v = "John"
            in pure v |> Expect.equal (Validation <| Ok v)
        ]
    , describe "extract"
        [ test "Extract a Result (Err) from Validation" <|
          \_ ->
            let res = Err "Please write a positive number"
                val = Validation <| res
            in extract val |> Expect.equal res
        , test "Extract a Result (Ok) from Validation" <|
          \_ ->
            let res = Ok 15
                val = Validation <| res
            in extract val |> Expect.equal res
        ]
    , describe "map"
        [ test "Change validated value" <|
          \_ ->
            let val = Validation <| Ok "John"
            in map String.toUpper val |> Expect.equal (Validation <| Ok "JOHN")
        , test "Keep Err value despite mapping" <|
          \_ ->
            let val = Validation <| Err "Wrong number"
            in map String.toUpper val |> Expect.equal val
        ]
    , describe "map2"
        [ test "Change validated value" <|
          \_ ->
            let val1 = Validation <| Ok "1"
                val2 = Validation <| Ok "2"
            in map2 String.append val1 val2 |> Expect.equal (Validation <| Ok "12")
        , test "Keep Err value despite mapping" <|
          \_ ->
            let val1 = Validation <| Err [ "Err1" ]
                val2 = Validation <| Err [ "Err2" ]
            in Validation (Err ["Err1", "Err2"]) |> Expect.equal (map2 String.append val1 val2)
        , test "Mapping doesn't happen if one of the values is an Err" <|
          \_ ->
            let val1 = Validation <| Err [ "Err1" ]
                val2 = Validation <| Ok "John"
            in Validation (Err ["Err1"]) |> Expect.equal (map2 String.append val1 val2)
        ]
    , describe "map3" <|
        let
          mapping a1 a2 a3 = String.append (String.append a1 a2) a3
        in
          [ test "Change validated value" <|
            \_ ->
              let val1 = Validation <| Ok "1"
                  val2 = Validation <| Ok "2"
                  val3 = Validation <| Ok "3"
              in map3 mapping val1 val2 val3 |> Expect.equal (Validation <| Ok "123")
          , test "Keep Err value despite mapping" <|
            \_ ->
              let val1 = Validation <| Err [ "Err1" ]
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Err [ "Err3" ]
              in Validation (Err ["Err1", "Err2", "Err3"]) |> Expect.equal (map3 mapping val1 val2 val3)
          , test "Mapping doesn't happen if one of the values is an Err" <|
            \_ ->
              let val1 = Validation <| Ok "Robert"
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Ok "John"
              in Validation (Err ["Err2"]) |> Expect.equal (map3 mapping val1 val2 val3)
          ]
    , describe "map4" <|
        let
          mapping a1 a2 a3 a4 = String.append (String.append (String.append a1 a2) a3) a4
        in
          [ test "Change validated value" <|
            \_ ->
              let val1 = Validation <| Ok "1"
                  val2 = Validation <| Ok "2"
                  val3 = Validation <| Ok "3"
                  val4 = Validation <| Ok "4"
              in map4 mapping val1 val2 val3 val4 |> Expect.equal (Validation <| Ok "1234")
          , test "Keep Err value despite mapping" <|
            \_ ->
              let val1 = Validation <| Err [ "Err1" ]
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Err [ "Err3" ]
                  val4 = Validation <| Err [ "Err4" ]
              in Validation (Err ["Err1", "Err2", "Err3", "Err4"]) |> Expect.equal (map4 mapping val1 val2 val3 val4)
          , test "Mapping doesn't happen if one of the values is an Err" <|
            \_ ->
              let val1 = Validation <| Ok "Robert"
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Ok "John"
                  val4 = Validation <| Ok "Bill"
              in Validation (Err ["Err2"]) |> Expect.equal (map4 mapping val1 val2 val3 val4)
          ]
    , describe "map5" <|
        let
          mapping a1 a2 a3 a4 a5 = String.append (String.append (String.append (String.append a1 a2) a3) a4) a5
        in
          [ test "Change validated value" <|
            \_ ->
              let val1 = Validation <| Ok "1"
                  val2 = Validation <| Ok "2"
                  val3 = Validation <| Ok "3"
                  val4 = Validation <| Ok "4"
                  val5 = Validation <| Ok "5"
              in map5 mapping val1 val2 val3 val4 val5 |> Expect.equal (Validation <| Ok "12345")
          , test "Keep Err value despite mapping" <|
            \_ ->
              let val1 = Validation <| Err [ "Err1" ]
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Err [ "Err3" ]
                  val4 = Validation <| Err [ "Err4" ]
                  val5 = Validation <| Err [ "Err5" ]
              in Validation (Err ["Err1", "Err2", "Err3", "Err4", "Err5"])
                  |> Expect.equal (map5 mapping val1 val2 val3 val4 val5)
          , test "Mapping doesn't happen if one of the values is an Err" <|
            \_ ->
              let val1 = Validation <| Ok "Robert"
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Ok "John"
                  val4 = Validation <| Ok "Bill"
                  val5 = Validation <| Ok "George"
              in Validation (Err ["Err2"]) |> Expect.equal (map5 mapping val1 val2 val3 val4 val5)
          ]
    , describe "map6" <|
        let
          mapping a1 a2 a3 a4 a5 a6 =
            String.append (String.append (String.append (String.append (String.append a1 a2) a3) a4) a5) a6
        in
          [ test "Change validated value" <|
            \_ ->
              let val1 = Validation <| Ok "1"
                  val2 = Validation <| Ok "2"
                  val3 = Validation <| Ok "3"
                  val4 = Validation <| Ok "4"
                  val5 = Validation <| Ok "5"
                  val6 = Validation <| Ok "6"
              in map6 mapping val1 val2 val3 val4 val5 val6 |> Expect.equal (Validation <| Ok "123456")
          , test "Keep Err value despite mapping" <|
            \_ ->
              let val1 = Validation <| Err [ "Err1" ]
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Err [ "Err3" ]
                  val4 = Validation <| Err [ "Err4" ]
                  val5 = Validation <| Err [ "Err5" ]
                  val6 = Validation <| Err [ "Err6" ]
              in Validation (Err ["Err1", "Err2", "Err3", "Err4", "Err5", "Err6"])
                  |> Expect.equal (map6 mapping val1 val2 val3 val4 val5 val6)
          , test "Mapping doesn't happen if one of the values is an Err" <|
            \_ ->
              let val1 = Validation <| Ok "Robert"
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Ok "John"
                  val4 = Validation <| Ok "Bill"
                  val5 = Validation <| Ok "George"
                  val6 = Validation <| Ok "Stuart"
              in Validation (Err ["Err2"]) |> Expect.equal (map6 mapping val1 val2 val3 val4 val5 val6)
          ]
    , describe "map7" <|
        let
          mapping a1 a2 a3 a4 a5 a6 a7 =
            String.append (String.append (String.append (String.append (String.append (String.append a1 a2) a3) a4) a5) a6) a7
        in
          [ test "Change validated value" <|
            \_ ->
              let val1 = Validation <| Ok "1"
                  val2 = Validation <| Ok "2"
                  val3 = Validation <| Ok "3"
                  val4 = Validation <| Ok "4"
                  val5 = Validation <| Ok "5"
                  val6 = Validation <| Ok "6"
                  val7 = Validation <| Ok "7"
              in map7 mapping val1 val2 val3 val4 val5 val6 val7 |> Expect.equal (Validation <| Ok "1234567")
          , test "Keep Err value despite mapping" <|
            \_ ->
              let val1 = Validation <| Err [ "Err1" ]
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Err [ "Err3" ]
                  val4 = Validation <| Err [ "Err4" ]
                  val5 = Validation <| Err [ "Err5" ]
                  val6 = Validation <| Err [ "Err6" ]
                  val7 = Validation <| Err [ "Err7" ]
              in Validation (Err ["Err1", "Err2", "Err3", "Err4", "Err5", "Err6", "Err7"])
                  |> Expect.equal (map7 mapping val1 val2 val3 val4 val5 val6 val7)
          , test "Mapping doesn't happen if one of the values is an Err" <|
            \_ ->
              let val1 = Validation <| Ok "Robert"
                  val2 = Validation <| Err [ "Err2" ]
                  val3 = Validation <| Ok "John"
                  val4 = Validation <| Ok "Bill"
                  val5 = Validation <| Ok "George"
                  val6 = Validation <| Ok "Stuart"
                  val7 = Validation <| Ok "James"
              in Validation (Err ["Err2"]) |> Expect.equal (map7 mapping val1 val2 val3 val4 val5 val6 val7)
          ]
    , describe "mapError" <|
        [ test "Change the value of the error, if it exists" <|
          \_ ->
            let val = Validation <| Err "Wrong number"
            in mapError String.toUpper val |> Expect.equal (Validation <| Err "WRONG NUMBER")
        , test "Don't change a value if a correct value is stored" <|
          \_ ->
            let val = Validation <| Ok "John"
            in mapError String.toUpper val |> (Expect.equal <| Validation <| Ok "John")
        ]
    , describe "andThen" <|
        [ test "Change the resulting value if all values are valid" <|
          \_ ->
            let val1 = Validation <| Ok "John"
                val2 = Validation <| Ok "Wick"
                val3 = Validation <| Ok "(Unicorn)"
                res = val1
                  |> andThen (\v1 -> val2
                  |> andThen (\v2 -> val3
                  |> andThen (\v3 -> pure <| String.join " " [ v1, v2, v3 ]
                  )))
                expected = Validation <| Ok "John Wick (Unicorn)"
            in res |> (Expect.equal expected)
        , test "If there are errors, it returns the first error" <|
          \_ ->
            let val1 = Validation <| Ok "One"
                val2 = Validation <| Ok "Two"
                val3 = Validation <| Err "Three"
                val4 = Validation <| Ok "Four"
                val5 = Validation <| Err "Five"
                val6 = Validation <| Ok "Six"
                res = val1
                  |> andThen (\v1 -> val2
                  |> andThen (\v2 -> val3
                  |> andThen (\v3 -> val4
                  |> andThen (\v4 -> val5
                  |> andThen (\v5 -> val6
                  |> andThen (\v6 -> pure <| String.join " " [ v1, v2, v3, v4, v5, v6 ]
                  ))))))
                expected = Validation <| Err "Three"
            in res |> (Expect.equal expected)
        ]
    , describe "withValidation" <|
        [ test "Perform a chain of calculations if all values are valid" <|
          \_ ->
            let firstName = Validation <| Ok "John"
                lastName = Validation <| Ok "Wick"
                age = Validation <| Ok 40
                email = Validation <| Ok "john@wick.com"
                ip = Validation <| Ok "70.235.93.79"
                res = pure ValidatedPerson
                        |> withValidation firstName
                        |> withValidation lastName
                        |> withValidation age
                        |> withValidation email
                        |> withValidation ip
                        |> toMaybe
                expected = Just <| ValidatedPerson "John" "Wick" 40 "john@wick.com" "70.235.93.79"
            in res |> (Expect.equal expected)
        , test "If more than one error has occurred, then accumulate them all" <|
          \_ ->
            let firstName = Validation <| Ok "John"
                lastName = Validation <| Err [ "Missing name" ]
                age = Validation <| Ok 40
                email = Validation <| Ok "john@wick.com"
                ip = Validation <| Err [ "Wrong IP address" ]
                res = pure ValidatedPerson
                        |> withValidation firstName
                        |> withValidation lastName
                        |> withValidation age
                        |> withValidation email
                        |> withValidation ip
                        |> extract
                        |> (\r -> case r of
                              Err l -> l
                              Ok _ -> ["Error: no error was intercepted in the calculations"]
                          )

                expected = ["Missing name", "Wrong IP address"]

            in res |> (Expect.equal expected)
        ]
    , describe "toMaybe" <|
        [ test "The Ok value in Validation should be wrapped in Just" <|
          \_ ->
            let val = Validation <| Ok "John"
            in toMaybe val |> Expect.equal (Just "John")
        , test "The Err value in Validation should be converted in Nothing" <|
          \_ ->
            let val = Validation <| Err [ "Wrong number" ]
            in toMaybe val |> Expect.equal Nothing
        ]
    , describe "fromMaybe" <|
        [ test "The Just value should be wrapper in Ok in Validation" <|
           \_ ->
            let val = Just 48
            in (fromMaybe val ["The age field is missing"]) |> Expect.equal (Validation <| Ok 48)
        , test "The Nothing value should be converted in Err in Validation with particular error messages" <|
           \_ ->
            let val = Nothing
            in (fromMaybe val ["The age field is missing"]) |> Expect.equal (Validation <| Err ["The age field is missing"])
        ]
    , describe "applyOrCollect" <|
        let
          flip : (a -> b -> c) -> b -> a -> c
          flip f b a = f a b

          withValidationDict : Validation (Dict.Dict comparable e) a
            -> Validation (Dict.Dict comparable e) (a -> b)
            -> Validation (Dict.Dict comparable e) b
          withValidationDict = flip <| applyOrCollect Dict.union
        in
          [ test "Using the Dict as an alternative structure to the List (Success result)" <|
            \_ ->
              let
                firstName = Validation <| Ok "John"
                lastName = Validation <| Ok "Wick"
                age = Validation <| Ok 40
                email = Validation <| Ok "john@wick.com"
                ip = Validation <| Ok "70.235.93.79"
                res = pure ValidatedPerson
                        |> withValidationDict firstName
                        |> withValidationDict lastName
                        |> withValidationDict age
                        |> withValidationDict email
                        |> withValidationDict ip
                        |> extract
                        |> Result.toMaybe

                expected = Just <| ValidatedPerson "John" "Wick" 40 "john@wick.com" "70.235.93.79"

              in res |> (Expect.equal expected)
          , test "Using the Dict as an alternative structure to the List (Failure result)" <|
            \_ ->
              let
                firstName = Validation <| Ok "John"
                lastName = Validation <| Ok "Wick"
                age = Validation <| Err <| Dict.singleton "age" "Empty field"
                email = Validation <| Ok "john@wick.com"
                ip = Validation <| Err <| Dict.singleton "ip" "Incorrect ip"
                res = pure ValidatedPerson
                        |> withValidationDict firstName
                        |> withValidationDict lastName
                        |> withValidationDict age
                        |> withValidationDict email
                        |> withValidationDict ip
                        |> extract
                        |> (\d -> case d of
                              Ok v -> Dict.empty
                              Err e -> e
                          )

                expected = Dict.fromList [ ("age", "Empty field"), ("ip", "Incorrect ip") ]

              in res |> (Expect.equal expected)
          ]
    ]

