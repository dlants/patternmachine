module Main.App where

import Matrix
import Array
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp

main = StartApp.start { model = model, view = view, update = update }

-- MODEL
model = Matrix.repeat 10 10 0

matrixRows matrix =
  List.filterMap (\i -> Matrix.getRow i matrix) [0..Matrix.height(matrix) - 1]

addColumn matrix =
  Matrix.repeat 1 (Matrix.height matrix) 0
  |> Matrix.concatHorizontal matrix
  |> Maybe.withDefault matrix

addRow matrix =
  Matrix.repeat (Matrix.width matrix) 1 0
  |> Matrix.concatVertical matrix
  |> Maybe.withDefault matrix

colors: Array.Array String
colors = Array.fromList [
  "blue",
  "green",
  "purple"
  ]

getColor val =
  Array.get (val % Array.length colors) colors
  |> Maybe.withDefault "red"

-- UPDATE
type Action = NoOp
            | Increment Int Int
            | AddRow
            | AddColumn

update action model =
  case action of
    NoOp -> model
    Increment row col ->
      Matrix.update col row (\val -> val + 1) model
    AddRow ->
      addRow model 
    AddColumn ->
      addColumn model

-- VIEW
boxStyle val =
  style [
    ("width", "50px"),
    ("height", "50px"),
    ("margin", "2px"),
    ("background-color", getColor val)
  ]

drawBox address rowidx colidx val =
  div [Html.Events.onClick address (Increment rowidx colidx)
    , boxStyle val
    ] []

drawRow address rowidx row =
  div [style [("display", "flex")]] (Array.toList (Array.indexedMap (drawBox address rowidx) row))

drawMatrix address matrix =
  div [] (List.indexedMap (drawRow address) (matrixRows matrix))

drawAddBox address clickAction =
  div [ Html.Events.onClick address clickAction
      , style [("background-color", "rgba(0,0,0,0.25)")
              , ("color", "white")
              ]
      ]
      [text "+"]

view address model =
  div [] [
    div [style [("display", "flex")]] [(drawMatrix address model)
           , (drawAddBox address AddColumn)
           ]
    , (drawAddBox address AddRow)
    ]
