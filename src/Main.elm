-- play with tiles
--


module Main exposing (..)

import Browser
import Color as SvgC
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (..)
import TypedSvg exposing (polygon, svg)
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Filters as SvgF
import TypedSvg.Filters.Attributes as SvgFA
import TypedSvg.Types as SvgT



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


type alias Model =
    { x : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = Default


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Default ->
            ( model, Cmd.none )



-- VIEW
-- -- #e67e80
-- red =
--     Element.rgb255 230 126 128


view : Model -> Browser.Document Msg
view _ =
    let
        -- #fdf6e3
        backgroundColor =
            Element.rgb255 253 246 227
    in
    { title = "Tiles"
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Background.color backgroundColor
            ]
          <|
            html <|
                svg
                    (fullscreenSvgBox 50 150)
                    [ TypedSvg.defs []
                        [ nullFilter
                        , tileClipPaths
                        ]
                    , TypedSvg.g
                        [ nullFilterStyle ]
                        tilesSvg
                    ]
        ]
    }


tilesSvg : List (Svg msg)
tilesSvg =
    [ parallelogramSvg 30 0 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 30 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 60 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 90 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 120 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 150 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 180 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 210 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 240 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 270 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 300 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 30 330 10.0 ( 0, 0 ) SvgC.red
    , parallelogramSvg 60 (-60 / 2) 10.0 ( 10, 0 ) SvgC.yellow
    , parallelogramSvg 45 0 10.0 ( 0, 50 ) SvgC.yellow
    , parallelogramSvg 45 45 10.0 ( 0, 50 ) SvgC.yellow
    , parallelogramSvg 45 90 10.0 ( 0, 50 ) SvgC.yellow
    , parallelogramSvg 45 135 10.0 ( 0, 50 ) SvgC.yellow
    , parallelogramSvg 45 180 10.0 ( 0, 50 ) SvgC.yellow
    , parallelogramSvg 45 225 10.0 ( 0, 50 ) SvgC.yellow
    , parallelogramSvg 45 270 10.0 ( 0, 50 ) SvgC.yellow
    , parallelogramSvg 45 315 10.0 ( 0, 50 ) SvgC.yellow
    , parallelogramSvg 60 0 10.0 ( 0, -50 ) SvgC.blue
    , parallelogramSvg 60 60 10.0 ( 0, -50 ) SvgC.blue
    , parallelogramSvg 60 120 10.0 ( 0, -50 ) SvgC.blue
    , parallelogramSvg 60 180 10.0 ( 0, -50 ) SvgC.blue
    , parallelogramSvg 60 240 10.0 ( 0, -50 ) SvgC.blue
    , parallelogramSvg 60 300 10.0 ( 0, -50 ) SvgC.blue
    ]



-- SVG Configuration
--
-- The parent SVG takes up the whole screen, and uses viewBox
-- to specify the coordiantes
-- https://ellie-app.com/cp8m289xjgQa1


fullscreenSvgBox : Float -> Float -> List (TypedSvg.Core.Attribute Msg)
fullscreenSvgBox width height =
    let
        offsetX =
            -width / 2

        offsetY =
            -height / 2
    in
    [ SvgA.width (SvgT.percent 100)
    , SvgA.height (SvgT.percent 100)
    , SvgA.viewBox offsetX offsetY width height
    ]


nullFilter : Svg msg
nullFilter =
    TypedSvg.filter
        [ SvgA.id "nullFilter" ]
        [ SvgF.blend
            [ SvgFA.in_ SvgT.InSourceGraphic
            , SvgFA.mode SvgT.ModeNormal -- https://stackoverflow.com/questions/47451435/svg-prevent-transparent-gaps-between-adjacent-polygons
            ]
            []
        ]


nullFilterStyle : TypedSvg.Core.Attribute msg
nullFilterStyle =
    SvgA.style "filter: url(#nullFilter)"


tileClipPaths : Svg msg
tileClipPaths =
    TypedSvg.defs []
        [ TypedSvg.clipPath [ SvgA.id <| "myClipPath30" ]
            [ parallelogramSvg 30 0 10.0 ( 0, 0 ) SvgC.lightRed ]
        , TypedSvg.clipPath [ SvgA.id <| "myClipPath45" ]
            [ parallelogramSvg 45 0 10.0 ( 0, 0 ) SvgC.lightRed ]
        , TypedSvg.clipPath [ SvgA.id <| "myClipPath60" ]
            [ parallelogramSvg 60 0 10.0 ( 0, 0 ) SvgC.lightRed ]
        ]



-- Geometry


type alias Radians =
    Float


type alias Degrees =
    Float


degreesToRadians : Degrees -> Radians
degreesToRadians degrees =
    degrees * (pi / 180)


type alias Point =
    ( Float, Float )


addPoints : Point -> Point -> Point
addPoints ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


type alias Quad =
    List Point


parallelogramVerts : Radians -> Float -> Quad
parallelogramVerts angle length =
    let
        --    c_______d
        --    /      /
        --   /      /
        --  /______/
        -- a       b
        a =
            ( 0, 0 )

        b =
            ( length, 0 )

        c =
            ( length * cos angle, length * sin angle )

        -- Adding vector AC to AB to get D
        d =
            addPoints b c
    in
    [ a, b, d, c ]



-- Geometry -> SVG


clipPath : Degrees -> String
clipPath angleDegrees =
    "myClipPath" ++ String.fromFloat angleDegrees


parallelogramSvg : Float -> Float -> Float -> Point -> SvgC.Color -> Svg msg
parallelogramSvg angleDegrees shapeRotationDegrees length ( coordX, coordY ) color =
    let
        angleRadians =
            degreesToRadians angleDegrees

        -- Calculate vertices
        quad =
            parallelogramVerts angleRadians length
    in
    polygon
        [ SvgA.points quad
        , SvgA.fill (SvgT.Paint color)
        , SvgA.stroke <| SvgT.Paint SvgC.black
        , SvgA.strokeWidth <| SvgT.pt 0.2
        , SvgA.style <|
            "paint-order:fill;"
                ++ " clip-path: url(#"
                ++ clipPath angleDegrees
                ++ ")"
        , SvgA.transform
            [ SvgT.Translate coordX coordY
            , SvgT.Rotate shapeRotationDegrees 0 0
            ]
        ]
        []
