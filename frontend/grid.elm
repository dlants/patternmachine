module Grid exposing (..)

import Array
import Matrix
import Window
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events


init : Window.Size -> Model
init windowSize =
    { matrix = (Matrix.repeat 10 10 0)
    , window = windowSize
    }



-- MODEL


type alias Model =
    { matrix : Matrix.Matrix Int
    , window : Window.Size
    }


matrixRows : Matrix.Matrix a -> List (Array.Array a)
matrixRows matrix =
    List.filterMap (\i -> Matrix.getRow i matrix) [0..Matrix.height (matrix) - 1]



-- +1 for the add row/col


boxsize : Model -> Int
boxsize model =
    let
        ( cols, rows ) =
            model.matrix.size
    in
        Basics.min (model.window.width // cols) (model.window.height // rows)


colors : Array.Array String
colors =
    Array.fromList
        [ "#e2e2e2"
        , "#c0504d"
        , "#4781b9"
        , "#1aad57"
        ]


getColor : Int -> String
getColor val =
    Array.get (val % Array.length colors) colors
        |> Maybe.withDefault "red"



-- UPDATE


type Msg
    = NoOp
    | Increment Int Int


increment : Int -> Int
increment val =
    (val + 1) % Array.length colors


update : Msg -> Model -> Model
update action model =
    let
        newModel =
            case action of
                NoOp ->
                    model

                Increment row col ->
                    { model | matrix = Matrix.update col row increment model.matrix }
    in
        newModel



-- VIEW


boxStyle : Int -> Int -> Attribute Msg
boxStyle val size =
    let
        sizeStr =
            (toString (size - 2)) ++ "px"
    in
        Attributes.style
            [ ( "width", sizeStr )
            , ( "height", sizeStr )
            , ( "margin", "1px" )
            , ( "background-color", getColor val )
            ]


drawBox : Int -> Int -> Int -> Int -> Html Msg
drawBox size rowidx colidx val =
    div
        [ Events.onClick (Increment rowidx colidx)
        , boxStyle val size
        ]
        []


drawRow : Int -> Int -> Array.Array Int -> Html Msg
drawRow size rowidx row =
    div [ Attributes.style [ ( "display", "flex" ) ] ] (Array.toList (Array.indexedMap (drawBox size rowidx) row))


view : Model -> Html Msg
view model =
    let
        size =
            boxsize model
    in
        div
            [ Attributes.style
                [ ( "display", "flex" )
                , ( "align-items", "center" )
                , ( "justify-content", "center" )
                , ( "position", "absolute" )
                , ( "top", "0px" )
                , ( "bottom", "0px" )
                , ( "left", "0px" )
                , ( "right", "0px" )
                , ( "background-color", getColor 0 )
                ]
            ]
            [ div [ Attributes.style [ ( "display", "flex" ) ] ]
                [ div []
                    (List.indexedMap (drawRow size) (matrixRows model.matrix))
                ]
            , a
                [ Attributes.href "/"
                , Attributes.style
                    [ ( "position", "absolute" )
                    , ( "bottom", "5px" )
                    , ( "right", "10px" )
                    , ( "color", "#666666" )
                    , ( "text-decoration", "none" )
                    ]
                ]
                [ text "reset" ]
            ]
