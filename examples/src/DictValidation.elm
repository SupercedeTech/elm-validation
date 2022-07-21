module DictValidation exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (onInput, onClick)
import Validation exposing (..)

type Msg
  = ChangeFirstName String
  | ChangeLastName String
  | ChangeAge String
  | ChangeEmail String
  | ChangeIpAddress String
  | Validate

type alias Model =
  { validatedPerson : Validation (Dict String String) Person
  , rawPerson : RawPerson
  , isShownValidatedPerson : Bool
  }

type alias RawPerson =
  { rpFirstName : String
  , rpLastName : String
  , rpAge : String
  , rpEmail : String
  , rpIpAddress : String
  }

type alias Person =
  { vpFirstName : String
  , vpLastName : String
  , vpAge : Int
  , vpEmail : String
  , vpIpAddress : String
  }

{-|

Please note, that this example demonstrates how to use the validator.
The implementations themselves are naive and not intended for use on production code.

|-}
main : Program () Model Msg
main = Browser.sandbox
  { init = initModel
  , view = view
  , update = update
  }

initModel : Model
initModel =
  { validatedPerson = Validation <| Ok <|
      { vpFirstName = ""
      , vpLastName = ""
      , vpAge = 0
      , vpEmail = ""
      , vpIpAddress = ""
      }
  , rawPerson =
      { rpFirstName = ""
      , rpLastName = ""
      , rpAge = ""
      , rpEmail = ""
      , rpIpAddress = ""
      }
  , isShownValidatedPerson = False
  }

update : Msg -> Model -> Model
update msg model = case msg of
  ChangeFirstName val ->
    let rawPerson = model.rawPerson
    in { model | rawPerson = { rawPerson | rpFirstName = val }}

  ChangeLastName val ->
    let rawPerson = model.rawPerson
    in { model | rawPerson = { rawPerson | rpLastName = val }}

  ChangeAge val ->
    let rawPerson = model.rawPerson
    in { model | rawPerson = { rawPerson | rpAge = val }}

  ChangeEmail val ->
    let rawPerson = model.rawPerson
    in { model | rawPerson = { rawPerson | rpEmail = val }}

  ChangeIpAddress val ->
    let rawPerson = model.rawPerson
    in { model | rawPerson = { rawPerson | rpIpAddress = val }}

  Validate ->
    let { rpFirstName, rpLastName, rpAge, rpEmail, rpIpAddress } = model.rawPerson
        applyValidationOn : String -> String -> List (String -> String -> Result (Dict String String) String) -> Validation (Dict String String) String
        applyValidationOn key field rules =
          let errors = Dict.fromList <| List.concat <| List.map Dict.toList <| List.map
                (\f -> case f field key of
                    Ok _ -> Dict.empty
                    Err e -> e
                ) rules
          in if Dict.isEmpty errors
             then Validation <| Ok field
             else Validation <| Err errors

        validatedFirstName = applyValidationOn "first-name" rpFirstName
          [ ifGreaterThanMaxLength 40 "Max length of First Name must be less than 40", ifBlank "First Name field is missing" ]
        validatedLastName = applyValidationOn "last-name" rpLastName
          [ ifLessThanMinLength 3 "Min length of Last Name must be greater than 3", ifBlank "Last Name field is missing" ]
        validatedAge = applyValidationOn "age" rpAge [ ifBlank "Age field is missing" ]
          |> andThen (\age -> fromMaybeDict (String.toInt age) (Dict.singleton "age" "Can't parse Age value as number"))
        validatedEmail = applyValidationOn "email" rpEmail [ ifBlank "Email field is missing", ifDoesntContainAt "Missing @ symbol" ]
        validatedIpAddress = applyValidationOn "ip" rpIpAddress [ ifBlank "Missing IP Address Field", ifNotValidIP "Invalid IP Address" ]

        res = pure Person
          |> withValidationDict validatedFirstName
          |> withValidationDict validatedLastName
          |> withValidationDict validatedAge
          |> withValidationDict validatedEmail
          |> withValidationDict validatedIpAddress

    in { model | validatedPerson = res, isShownValidatedPerson = True }

view : Model -> Html Msg
view model =
  let
    fieldStyles = [ style "display" "flex", style "justify-content" "space-between", style "margin-top" "15px" ]
  in
    div []
        [ div
            [ style "max-width" "300px", style "padding" "40px" ]
            [ div fieldStyles
                  [ span [] [ text "First Name" ]
                  , input [ placeholder "First Name", onInput ChangeFirstName, value model.rawPerson.rpFirstName ] []
                  ]
            , div fieldStyles
                  [ span [] [ text "Last Name" ]
                  , input [ placeholder "Last Name", onInput ChangeLastName, value model.rawPerson.rpLastName ] []
                  ]
            , div fieldStyles
                  [ span [] [ text "Age" ]
                  , input [ placeholder "Age", onInput ChangeAge, value model.rawPerson.rpAge ] []
                  ]
            , div fieldStyles
                  [ span [] [ text "Email" ]
                  , input [ placeholder "Email", onInput ChangeEmail, value model.rawPerson.rpEmail ] []
                  ]
            , div fieldStyles
                  [ span [] [ text "IP Address" ]
                  , input [ placeholder "IP", onInput ChangeIpAddress, value model.rawPerson.rpIpAddress ] []
                  ]
            , button [ style "margin-top" "20px", onClick Validate ] [ text "Validate" ]
            ]
        , if model.isShownValidatedPerson
          then
            div [ style "padding" "40px", style "padding-top" "0" ]
                [ span [] [ text "Result of validation:" ]
                , div [ style "color" (if isNothing <| toMaybeDict model.validatedPerson then "#FF6347" else "#6B8E23")
                      , style "margin-top" "20px" ]
                      [ text <| Debug.toString model.validatedPerson ]
                ]
          else text ""
        ]

ifBlank : String -> String -> String -> Result (Dict String String) String
ifBlank message val key =
  if String.isEmpty val
  then Err <| Dict.singleton key message
  else Ok val

ifGreaterThanMaxLength : Int -> String -> String -> String -> Result (Dict String String) String
ifGreaterThanMaxLength maxLength message val key =
  if String.length val > maxLength
  then Err <| Dict.singleton key message
  else Ok val

ifLessThanMinLength : Int -> String -> String -> String -> Result (Dict String String) String
ifLessThanMinLength minLength message val key =
  if String.length val < minLength
  then Err <| Dict.singleton key message
  else Ok val

ifDoesntContainAt : String -> String -> String -> Result (Dict String String) String
ifDoesntContainAt message val key =
  if String.contains "@" val
  then Ok val
  else Err <| Dict.singleton key message

ifNotValidIP : String -> String -> String -> Result (Dict String String) String
ifNotValidIP message val key =
  if String.all (\x -> Char.isDigit x || x == '.') val
  then Ok val
  else Err <| Dict.singleton key message

isNothing : Maybe a -> Bool
isNothing m = case m of
  Nothing -> True
  Just _ -> False

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

withValidationDict : Validation (Dict.Dict comparable e) a
  -> Validation (Dict.Dict comparable e) (a -> b)
  -> Validation (Dict.Dict comparable e) b
withValidationDict = flip <| applyOrCollect Dict.union

fromMaybeDict : Maybe a -> Dict ek ev -> Validation (Dict ek ev) a
fromMaybeDict mValue errors =
  case mValue of
    Nothing -> Validation <| Err errors

    Just value -> Validation <| Ok value

toMaybeDict : Validation (Dict ek ev) a -> Maybe a
toMaybeDict v =
  case v of
  Validation (Ok value) -> Just value

  Validation (Err _) -> Nothing
