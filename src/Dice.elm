-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--


module Dice exposing (..)

import Browser
import Element exposing (..)
import Element.Events exposing (..)
import Element.Border as Border
import Element.Input exposing (button)
import Random
import Svg exposing (Svg, circle, rect, svg)
import Svg.Attributes as SvgA
import Time
import Element.Font exposing (alignRight)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Status
    = IsRolling
    | Rolled


type alias Model =
    { dieFace : Int
    , state : Status
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1 Rolled
    , Cmd.none
    )



-- UPDATE


type Msg
    = StartRoll
    | EndRoll
    | NewFace Int
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.state of
                IsRolling ->
                    ( Model (modBy 6 (model.dieFace + 1)) IsRolling
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StartRoll ->
            ( Model -1 IsRolling
            , Cmd.none
            )

        EndRoll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( Model newFace Rolled
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 50 Tick



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "dice"
    , body =
        [ Element.layout [ Element.width Element.fill, Element.height Element.fill ]
            (column [ Element.centerX, Element.centerY ]
                [ case model.state of
                    Rolled ->
                        dieFace model.dieFace

                    IsRolling ->
                        dieFace (model.dieFace + 1)
                , el [ centerX ]
                    (button
                        [ padding 5
                        , Border.width 1
                        , Border.rounded 5
                        , onMouseDown StartRoll
                        , onMouseUp EndRoll
                        ]
                        { label = Element.text "Roll", onPress = Nothing }
                    )
                ]
            )
        ]
    }



-- Function to create a single dot


dot : ( String, String ) -> Svg msg
dot ( x, y ) =
    circle [ SvgA.cx x, SvgA.cy y, SvgA.r "5", SvgA.fill "#000" ] []


dieDots : Int -> List ( String, String )
dieDots n =
    case n of
        1 ->
            [ ( "50", "50" ) ]

        2 ->
            [ ( "30", "30" ), ( "70", "70" ) ]

        3 ->
            [ ( "30", "30" ), ( "50", "50" ), ( "70", "70" ) ]

        4 ->
            [ ( "30", "30" ), ( "30", "70" ), ( "70", "30" ), ( "70", "70" ) ]

        5 ->
            [ ( "30", "30" ), ( "30", "70" ), ( "50", "50" ), ( "70", "30" ), ( "70", "70" ) ]

        6 ->
            [ ( "30", "30" ), ( "30", "50" ), ( "30", "70" ), ( "70", "30" ), ( "70", "50" ), ( "70", "70" ) ]

        _ ->
            []



-- Default case, should not happen for valid input


dieFace : Int -> Element msg
dieFace n =
    html <|
        svg [ SvgA.width "100", SvgA.height "100", SvgA.viewBox "0 0 100 100" ]
            (rect
                [ SvgA.x "10"
                , SvgA.y "10"
                , SvgA.width "80"
                , SvgA.height "80"
                , SvgA.fill "#fff"
                , SvgA.stroke "#000"
                , SvgA.rx "10"
                , SvgA.strokeWidth "5"
                ]
                []
                :: List.map dot (dieDots n)
            )
