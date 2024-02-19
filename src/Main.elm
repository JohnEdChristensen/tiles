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


type alias TileType =
    { angleDegrees : Degrees
    , length : Float
    , color : SvgC.Color
    }


type alias Tile =
    { tileType : TileType
    , location : Point
    , rotation : Degrees
    }


tile30 : TileType
tile30 =
    { angleDegrees = 30.0
    , length = 10
    , color = SvgC.red
    }


baseTile30 : Tile
baseTile30 =
    { tileType = tile30
    , location = ( 0, 0 )
    , rotation = 0
    }


tile45 : TileType
tile45 =
    { angleDegrees = 45.0
    , length = 10
    , color = SvgC.yellow
    }


baseTile45 : Tile
baseTile45 =
    { tileType = tile45
    , location = ( 0, 0 )
    , rotation = 0
    }


tile60 : TileType
tile60 =
    { angleDegrees = 60.0
    , length = 10
    , color = SvgC.blue
    }


baseTile60 : Tile
baseTile60 =
    { tileType = tile60
    , location = ( 0, 0 )
    , rotation = 0
    }


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
            tileCanvasEl
        ]
    }


tileCanvasEl : Element Msg
tileCanvasEl =
    html <|
        svg
            (fullscreenSvgBox 50 150)
            [ TypedSvg.defs []
                [ nullFilter
                , tileClipPaths
                ]
            , TypedSvg.g
                [ nullFilterStyle
                ]
               tile360Svg
            ]


rotateAboutSelf : Tile -> Degrees -> Tile
rotateAboutSelf tile rotationDegrees =
    { tile | rotation = rotationDegrees }


rotatePoint : Point -> Degrees -> Point
rotatePoint ( x, y ) angle =
    let
        angleR =
            degrees angle
    in
    ( x * cos angleR - y * sin angleR
    , x * sin angleR + y * cos angleR
    )


rotateAboutOrigin : Tile -> Degrees -> Tile
rotateAboutOrigin tile angle =
    { tile
        | rotation = tile.rotation + angle
        , location = rotatePoint tile.location angle
    }


copyAroundOrigin : Degrees -> Tile -> List Tile
copyAroundOrigin stepSize tile =
    let
        angles =
            generateAngles ( 0, 360 ) (round stepSize)
    in
    List.map (rotateAboutOrigin tile) angles


generateAngles : ( Int, Int ) -> Int -> List Degrees
generateAngles ( start, stop ) step =
    List.range start (stop // step)
        |> List.map (\n -> toFloat (n * step))


tile360Svg : List (Svg msg)
tile360Svg =
    let
        cluster30 =
            baseTile30

        cluster45 =
            { baseTile45 | location = ( -40, 0 ) }

        cluster60 =
            { baseTile60 | location = ( 40, 0 ) }
    in
    List.map tileToSvg <|
        List.map
            (rotateAboutSelf cluster30)
            (generateAngles ( 0, 360 ) 30)
            ++ List.map
                (rotateAboutSelf cluster45)
                (generateAngles ( 0, 360 ) 45)
            ++ List.map
                (rotateAboutSelf cluster60)
                (generateAngles ( 0, 360 ) 60)


tilesFun : List (Svg msg)
tilesFun =
    let
        r1 =
            baseTile30

        r1Quad =
            tileToQuad r1

        b1 =
            { baseTile60 | location = r1Quad.b, rotation = 30 }

        b1Quad =
            tileToQuad b1

        y1 =
            { baseTile45 | location = b1Quad.b, rotation = 15 }

        y1Quad =
            tileToQuad y1

        y2 =
            { y1 | rotation = 60 }

        y2Quad =
            tileToQuad y1

        r2 =
            { baseTile30 | location = b1Quad.d, rotation = 15 }

        y3 =
            { baseTile45 | location = y1Quad.b, rotation = 15 }

        y4 =
            { baseTile45 | location = y2Quad.b, rotation = 60 }
    in
    List.map tileToSvg <|
            List.concatMap (copyAroundOrigin 30)
                [ r1, b1, y1, y2, r2, y3, y4 ]



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



-- This filter helps alleviate small gaps between tiles. Maybe caused by floating point errors?
-- https://stackoverflow.com/questions/47451435/svg-prevent-transparent-gaps-between-adjacent-polygons


nullFilter : Svg msg
nullFilter =
    TypedSvg.filter
        [ SvgA.id "nullFilter" ]
        [ SvgF.blend
            [ SvgFA.in_ SvgT.InSourceGraphic
            , SvgFA.mode SvgT.ModeNormal
            ]
            []
        ]


nullFilterStyle : TypedSvg.Core.Attribute msg
nullFilterStyle =
    SvgA.style "filter: url(#nullFilter)"


tileClipPaths : Svg msg
tileClipPaths =
    TypedSvg.defs []
        [ TypedSvg.clipPath [ SvgA.id <| clipPathId 30 ]
            [ tileToSvg baseTile30 ]
        , TypedSvg.clipPath [ SvgA.id <| clipPathId 45 ]
            [ tileToSvg baseTile45 ]
        , TypedSvg.clipPath [ SvgA.id <| clipPathId 60 ]
            [ tileToSvg baseTile60 ]
        ]



-- Geometry


type alias Radians =
    Float


type alias Degrees =
    Float



-- degreesToRadians : Degrees -> Radians
-- degreesToRadians degrees =
--     degrees * (pi / 180)


type alias Point =
    ( Float, Float )


addPoints : Point -> Point -> Point
addPoints ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


type alias Quad =
    { a : Point, b : Point, c : Point, d : Point }


quadToList : Quad -> List Point
quadToList { a, b, c, d } =
    [ a, b, d, c ]



-- Counter clockwise


parallelogramVerts : Degrees -> Float -> Quad
parallelogramVerts angle length =
    let
        --    c_______d
        --    /      /
        --   /      /
        -- a/______/b
        --     ^
        --   length
        --
        -- angle = /_cab
        a =
            ( 0, 0 )

        b =
            ( length, 0 )

        c =
            --( length * cos angle, length * sin angle )
            rotatePoint b -angle

        -- Adding vector AC to AB to get D
        d =
            addPoints b c
    in
    { a = a, b = b, d = d, c = c }



-- Geometry -> SVG


tileToQuad : Tile -> Quad
tileToQuad { tileType, location, rotation } =
    let
        quad =
            parallelogramVerts tileType.angleDegrees tileType.length

        ap =
            rotatePoint quad.a rotation
                |> addPoints location

        bp =
            rotatePoint quad.b rotation
                |> addPoints location

        cp =
            rotatePoint quad.c rotation
                |> addPoints location

        dp =
            rotatePoint quad.d rotation
                |> addPoints location
    in
    { a = ap, b = bp, c = cp, d = dp }


tileToSvg : Tile -> Svg msg
tileToSvg { tileType, location, rotation } =
    let
        coordX =
            Tuple.first location

        coordY =
            Tuple.second location

        -- Calculate vertices
        quad =
            parallelogramVerts tileType.angleDegrees tileType.length
    in
    polygon
        [ SvgA.points (quadToList quad)
        , SvgA.fill (SvgT.Paint tileType.color)
        , SvgA.stroke <| SvgT.Paint SvgC.black
        , SvgA.strokeWidth <| SvgT.pt 0.2
        , SvgA.style <|
            "paint-order:fill;"
                ++ " clip-path: url(#"
                ++ clipPathId tileType.angleDegrees
                ++ ")"
        , SvgA.transform
            [ SvgT.Translate coordX coordY
            , SvgT.Rotate rotation 0 0
            ]
        ]
        []


clipPathId : Degrees -> String
clipPathId angleDegrees =
    "TileClipPath" ++ String.fromFloat angleDegrees
