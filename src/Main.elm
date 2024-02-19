-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--


module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE
-- one update message for each out our model fields


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- record update syntax
        -- say "model where name is newName"
        -- say "model where field is value"
        Name newName ->
            { model | name = newName }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []



--attempt 1
{- viewValidation : Model -> Html msg
   viewValidation model =
       if model.password == model.passwordAgain then
           if String.length model.password >= 8 then
               if String.any isUpper model.password then
                   if String.any isLower model.password then
                       div [ style "color" "green" ] [ text "OK" ]

                   else
                       div [ style "color" "red" ] [ text "needs at least 1 lower!" ]

               else
                   div [ style "color" "red" ] [ text "needs at least 1 upper!" ]

           else
               div [ style "color" "red" ] [ text "Password must be at least 8 chars!" ]

       else
           div [ style "color" "red" ] [ text "Passwords do not match!" ]
-}
--attempt 2, don't nest if else, more readable
{- viewValidation : Model -> Html msg
   viewValidation model =
       if not (model.password == model.passwordAgain) then
           div [ style "color" "red" ] [ text "Passwords do not match!" ]

       else if not (String.length model.password >= 8) then
           div [ style "color" "red" ] [ text "Password must be at least 8 chars!" ]

       else if not (String.any isUpper model.password) then
           div [ style "color" "red" ] [ text "needs at least 1 upper!" ]

       else if not (String.any isLower model.password) then
           div [ style "color" "red" ] [ text "needs at least 1 lower!" ]

       else
           div [ style "color" "green" ] [ text "OK" ]

-}
--attempt 3 too large of a tuple
-- viewValidation : Model -> Html msg
-- viewValidation model =
--     case ( model.password == model.passwordAgain, String.length model.password >= 8, String.any Char.isUpper model.password, String.any Char.iusLower model.password ) of
--         ( False, _, _, _ ) ->
--             div [ style "color" "red" ] [ text "Passwords do not match!" ]
--
--         ( True, False, _, _ ) ->
--             div [ style "color" "red" ] [ text "Password must be at least 8 chars!" ]
--
--         ( True, True, False, _ ) ->
--             div [ style "color" "red" ] [ text "needs at least 1 upper!" ]
--
--         ( True, True, True, False ) ->
--             div [ style "color" "red" ] [ text "needs at least 1 lower!" ]
--
--         ( True, True, True, True ) ->
--             div [ style "color" "green" ] [ text "OK" ]
--attempt 4


type ValidationResult
    = Valid
    | Invalid (List String)


passwordsMatch : String -> String -> Bool
passwordsMatch password passwordAgain =
    password == passwordAgain


passwordLongEnough : String -> Bool
passwordLongEnough password =
    String.length password >= 8


containsUpper : String -> Bool
containsUpper password =
    String.any Char.isUpper password


containsLower : String -> Bool
containsLower password =
    String.any Char.isLower password


validatePassword : Model -> ValidationResult
validatePassword model =
    let
        errors =
            [ if not (passwordsMatch model.password model.passwordAgain) then
                Just "Passwords do not match!"

              else
                Nothing
            , if not (passwordLongEnough model.password) then
                Just "Password must be at least 8 characters long."

              else
                Nothing
            , if not (containsUpper model.password) then
                Just "Password must contain at least 1 uppercase"

              else
                Nothing
            , if not (containsLower model.password) then
                Just "Password must contain at least 1 lowercase"

              else
                Nothing

            -- Add other checks here
            ]
    in
    case List.filterMap identity errors of
        [] ->
            Valid

        errs ->
            Invalid errs


viewValidation : Model -> Html msg
viewValidation model =
    case validatePassword model of
        Valid ->
            div
                [ style "background" "lime"
                , style "width" "fit-content"
                ]
                [ viewMessage "green" "OK" ]

        Invalid reasons ->
            div
                [ style "background" "pink"
                , style "width" "fit-content"
                ]
                (List.map (viewMessage "red") reasons)


viewMessage : String -> String -> Html abc
viewMessage color message =
    div [ style "color" color ] [ text message ]

-- validatePassword : Model -> ValidationResult
-- validatePassword model =
--     let
--         errors =
--             [ if not (passwordsMatch model.password model.passwordAgain) then
--                 Just "Passwords do not match!"
--               else
--                 Nothing
--             , if not (passwordLongEnough model.password) then
--                 Just "Password must be at least 8 characters long."
--               else
--                 Nothing
--             -- Add other checks here
--             ]
--     in
--     case List.filterMap identity errors of
--         [] -> Valid
--         errs -> Invalid errs
