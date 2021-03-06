module Grid (..) where

import Effects
import Window
import Matrix
import Array
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp


init windowSize =
  ( { matrix = (Matrix.repeat 10 10 0)
    , window = windowSize
    }
  , Effects.none
  )



-- MODEL


type alias Model =
  { matrix : Matrix.Matrix Int
  , window : ( Int, Int )
  }


matrixRows matrix =
  List.filterMap (\i -> Matrix.getRow i matrix) [0..Matrix.height (matrix) - 1]


addColumn matrix =
  Matrix.repeat 1 (Matrix.height matrix) 0
    |> Matrix.concatHorizontal matrix
    |> Maybe.withDefault matrix


addRow matrix =
  Matrix.repeat (Matrix.width matrix) 1 0
    |> Matrix.concatVertical matrix
    |> Maybe.withDefault matrix



-- +1 for the add row/col


boxsize : Model -> Int
boxsize model =
  let
    ( width, height ) =
      model.window

    ( cols, rows ) =
      model.matrix.size
  in
    Basics.min (width // cols) (height // rows)


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


getColor val =
  Array.get (val % Array.length colors) colors
    |> Maybe.withDefault "red"



-- UPDATE


type Action
  = NoOp
  | Increment Int Int
  | AddRow
  | AddColumn
  | Resize ( Int, Int )


update action model =
  let
    newModel =
      case action of
        NoOp ->
          model

        Increment row col ->
          { model | matrix = Matrix.update col row (\val -> val + 1) model.matrix }

        AddRow ->
          { model | matrix = addRow model.matrix }

        AddColumn ->
          { model | matrix = addColumn model.matrix }

        Resize window ->
          { model | window = window }
  in
    ( newModel, Effects.none )



-- VIEW


boxStyle val size =
  let
    sizeStr =
      (toString (size - 2)) ++ "px"
  in
    style
      [ ( "width", sizeStr )
      , ( "height", sizeStr )
      , ( "margin", "1px" )
      , ( "background-color", getColor val )
      ]


drawBox address size rowidx colidx val =
  div
    [ Html.Events.onClick address (Increment rowidx colidx)
    , boxStyle val size
    ]
    []


drawRow address size rowidx row =
  div [ style [ ( "display", "flex" ) ] ] (Array.toList (Array.indexedMap (drawBox address size rowidx) row))


drawMatrix address size matrix =
  div [] (List.indexedMap (drawRow address size) (matrixRows matrix))


drawAddBox address clickAction =
  div
    [ Html.Events.onClick address clickAction
    , style
        [ ( "background-color", "rgba(0,0,0,0.25)" )
        , ( "color", "white" )
        ]
    ]
    [ text "+" ]


view address model =
  let
    size =
      boxsize model
  in
    div
      [ style
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
          [ style [ ( "display", "flex" ) ] ]
          [ (drawMatrix address size model.matrix) ]
      ]
