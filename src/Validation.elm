module Validation exposing
  ( Validation(..)
  , withValidation
  , pure
  , map
  , map2
  , map3
  , map4
  , map5
  , map6
  , map7
  , andThen
  , fromMaybe
  , toMaybe
  , mapError
  , applyOrCollect
  , extract
  )

{-| This module contains a set of functions for data validation.

@docs Validation
@docs pure
@docs extract
@docs andThen
@docs withValidation
@docs applyOrCollect
@docs map
@docs map2
@docs map3
@docs map4
@docs map5
@docs map6
@docs map7
@docs mapError
@docs toMaybe
@docs fromMaybe

-}

{-| A type like result but accumulates errors.
    normal usage would entail `map`, `pure` and `withValidation`.

    `andThen` can be used if we depend on the result of a previous
    validation, this will cause incremental error collection however.

    This is inspired by the Haskell equivalent: https://hackage.haskell.org/package/validation
-}
type Validation e r = Validation (Result e r)

{-| Wrap some value in Validation

  Wrap in Validation data constructor

    pure "John" == Validation <| Ok "John"

-}
pure : a -> Validation e a
pure x = Validation <| Ok x

{-| Result extraction

  Extract a Result from Validation

    extract <| Validation <| Err "Wrong number" == Err "Wrong number"
    extract <| Validation <| Ok "John" == Ok "John"

-}
extract : Validation e r -> Result e r
extract (Validation a) = a

{-| Combines two validations into one, and gets an opportunity to
    collect the errors with the given function.
    This is preferable over `andThen` because you can collect errors.
    typically usage trough `withValidation`, but `applyOrCollect`
    is exposed to allow usage of different data structures. For example:

        withValidationDict : Validation (Dict comparable e) (a -> b)
        -> Validation (Dict comparable e) a
        -> Validation (Dict comparable e) b
        withValidationDict = applyOrCollect Dict.union

-}
applyOrCollect : (e -> e -> e) -> Validation e (a -> b) -> Validation e a -> Validation e b
applyOrCollect semigroupF funVal argVal =
  case (funVal, argVal) of
    (Validation (Err e1), Validation (Err e2)) -> Validation <| Err <| semigroupF e1 e2
    (Validation (Err e1), Validation (Ok _)) -> Validation <| Err e1
    (Validation (Ok _), Validation (Err e2)) -> Validation <| Err e2
    (Validation (Ok fun), Validation (Ok arg)) -> Validation <| Ok (fun arg)

{-| Validated value mapping

  Mapping a validated value

    map toUpper <| Validation <| Ok "John" == Validation <| Ok "JOHN"
    map toUpper <| Validation <| Err "Wrong number" == Validation <| Err "Wrong Number"

-}
map : (a -> b) -> Validation e a -> Validation e b
map f v =
  case v of
    Validation (Err e) -> Validation (Err e)
    Validation (Ok d) -> Validation (Ok (f d))

{-|
-}
map2 : (a1 -> a2 -> a3)
    -> (Validation (List e) a1)
    -> (Validation (List e) a2)
    -> (Validation (List e) a3)
map2 f a = applyOrCollect (++) (map f a)

{-|
-}
map3 : (a1 -> a2 -> a3 -> a4)
    -> (Validation (List e) a1)
    -> (Validation (List e) a2)
    -> (Validation (List e) a3)
    -> (Validation (List e) a4)
map3 f a1 a2 = applyOrCollect (++) (map2 f a1 a2)

{-|
-}
map4 : (a1 -> a2 -> a3 -> a4 -> a5)
    -> (Validation (List e) a1)
    -> (Validation (List e) a2)
    -> (Validation (List e) a3)
    -> (Validation (List e) a4)
    -> (Validation (List e) a5)
map4 f a1 a2 a3 = applyOrCollect (++) (map3 f a1 a2 a3)

{-|
-}
map5 : (a1 -> a2 -> a3 -> a4 -> a5 -> a6)
    -> (Validation (List e) a1)
    -> (Validation (List e) a2)
    -> (Validation (List e) a3)
    -> (Validation (List e) a4)
    -> (Validation (List e) a5)
    -> (Validation (List e) a6)
map5 f a1 a2 a3 a4 = applyOrCollect (++) (map4 f a1 a2 a3 a4)

{-|
-}
map6 : (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7)
    -> (Validation (List e) a1)
    -> (Validation (List e) a2)
    -> (Validation (List e) a3)
    -> (Validation (List e) a4)
    -> (Validation (List e) a5)
    -> (Validation (List e) a6)
    -> (Validation (List e) a7)
map6 f a1 a2 a3 a4 a5 = applyOrCollect (++) (map5 f a1 a2 a3 a4 a5)

{-|
-}
map7 : (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8)
    -> (Validation (List e) a1)
    -> (Validation (List e) a2)
    -> (Validation (List e) a3)
    -> (Validation (List e) a4)
    -> (Validation (List e) a5)
    -> (Validation (List e) a6)
    -> (Validation (List e) a7)
    -> (Validation (List e) a8)
map7 f a1 a2 a3 a4 a5 a6 = applyOrCollect (++) (map6 f a1 a2 a3 a4 a5 a6)

{-| Error mapping

    mapError toUpper <| Validation <| Err "Wrong number" == Validation <| Err "WRONG NUMBER"
    mapError toUpper <| Validation <| Ok "John" == Validation <| Ok "John"

-}
mapError : (e -> e1) -> Validation e a -> Validation e1 a
mapError f v =
    case v of
    Validation (Err e) -> Validation (Err (f e))
    Validation (Ok d) -> Validation (Ok d)

{-| Chaining computations

  Chain together many computations that may failed.

    val1 = Validation <| Ok "One"
    val2 = Validation <| Ok "Two"
    val3 = Validation <| Ok "Three"
    res = val1
      |> andThen (\v1 -> val2
      |> andThen (\v2 -> val3
      |> andThen (\v3 -> pure <| String.join " " [ v1, v2, v3 ]
    )))
    -- `res` is `Validation <| Ok "One Two Three"`

  Note that if one of the values in the chain is an error, the result will be the first error encountered.
  If you want to accumulate errors, then use the @withValidation function

    type alias ValidatedPerson =
      { firstName : String
      , lastName : String
      , age : Integer
      , email : String
      , ipAddress : String
      }

    let
        validatedFirstName = Validation <| Ok "John"
        validatedLastName  = Validation <| Err "The last name field is missing"
        validatedAge       = Validation <| Ok 40
        validatedEmail     = Validation <| Err "Invalid email: johnwick.com"
        validatedIpAddress = Validation <| Ok "70.235.93.79"
        res = validatedFirstName
          |> andThen (\vFn -> validatedLastName
          |> andThen (\fLn -> validatedAge
          |> andThen (\vAge -> validatedEmail
          |> andThen (\vEmail -> validatedIpAddress
          |> andThen (\vIp -> pure <| ValidatedPerson vId vFn vLn vAge vEmail vIp
          )))))
        -- `res` is equal to `Validation <| Err "The last name field is missing"
-}
andThen : (a -> Validation e b) -> Validation e a -> Validation e b
andThen f result =
  case result of
    Validation (Ok value) -> f value
    Validation (Err msg) -> Validation <| Err msg

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

{-| Chaining computations with error accumulation

  Chain together many computations that may failed. Errors are stored in a list.
  If you want to display only the first encountered error, use `andThen`.
  If you don't want to work with lists, you can create your own function to do a chain of calculations. To do this, see the `@applyOrCollect` function

    type alias ValidatedPerson =
      { firstName : String
      , lastName : String
      , age : Int
      , email : String
      , ipAddress : String
      }

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
        -- `res` is equal to `Validation <| Ok <| ValidatedPerson { ... }

  If there are errors:

    let firstName = Validation <| Ok "John"
        lastName = Validation <| Ok "Wick"
        age = Validation <| Err ["Invalid age"]
        email = Validation <| Ok "john@wick.com"
        ip = Validation <| Err ["Invalid ip"]
        res = pure ValidatedPerson
                |> withValidation firstName
                |> withValidation lastName
                |> withValidation age
                |> withValidation email
                |> withValidation ip
        -- `res` is equal to `Validation <| Err ["Invalid age", "Invalid ip"]

-}
withValidation : Validation (List e) a
  -> Validation (List e) (a -> b)
  -> Validation (List e) b
withValidation = flip (applyOrCollect (++))

{-| Converting Maybe to Validation

    fromMaybe Nothing ["The age field is missing"] == Validation <| Err ["The field age is missing"]
    fromMaybe (Just 48) ["The age field is missing"] == Validation <| Ok 48

-}
fromMaybe : Maybe a -> List e -> Validation (List e) a
fromMaybe mValue errors =
  case mValue of
    Nothing -> Validation <| Err errors

    Just value -> Validation <| Ok value

{-| Converting Validation to Maybe

    toMaybe <| Validation <| Err ["The field age is missing"] == Nothing
    toMaybe <| Validation <| Ok 48 == Just 48

-}
toMaybe : Validation (List e) a -> Maybe a
toMaybe v =
  case v of
  Validation (Ok value) -> Just value

  Validation (Err _) -> Nothing
