module Grid exposing (..)

import Array
import Matrix
import Window
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events


init: Window.Size -> Model
init windowSize =
    { matrix = (Matrix.repeat 10 10 0)
    , window = windowSize
    }


-- MODEL

type alias Model =
  { matrix : Matrix.Matrix Int
  , window : Window.Size
  }

matrixRows: Matrix.Matrix a -> List (Array.Array a)
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
    [ "#22a68a"
    , "#7eb7d5"
    , "#b696c8"
    ]



{- -
, "#bdd730"
, "#9bccb4"
, "#fedf4a"
, "#556896"
, "#f28f4a"
, "#e93830"
, "#f6dec6"
]-
-}


getColor: Int -> String
getColor val =
  Array.get (val % Array.length colors) colors
    |> Maybe.withDefault "red"

-- UPDATE


type Msg
  = NoOp
  | Increment Int Int

update: Msg -> Model -> Model
update action model =
  let
    newModel =
      case action of
        NoOp ->
          model

        Increment row col ->
          { model | matrix = Matrix.update col row (\val -> val + 1) model.matrix }
  in
    newModel


-- VIEW
boxStyle: Int -> Int -> Attribute Msg
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

drawBox: Int -> Int -> Int -> Int -> Html Msg
drawBox size rowidx colidx val =
  div
    [ Events.onClick (Increment rowidx colidx)
    , boxStyle val size
    ]
    []

drawRow: Int -> Int -> Array.Array Int -> Html Msg
drawRow size rowidx row =
  div [ Attributes.style [ ( "display", "flex" ) ] ] (Array.toList (Array.indexedMap (drawBox size rowidx) row))


view: Model -> Html Msg
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
          ]
      ]
      [ div
          [ Attributes.style [ ( "display", "flex" ) ] ]
          [ div
              []
              (List.indexedMap (drawRow size) (matrixRows model.matrix))
          ]
      ]
