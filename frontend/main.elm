module PatternMachine (..) where

import Window
import StartApp
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Grid exposing (Model, init, Action, update, view)


app =
  StartApp.start
    { init = init windowSize
    , view = view
    , update = update
    , inputs = [ resize ]
    }


main =
  app.html


port windowSize : ( Int, Int )



-- INPUTS


resize : Signal Action
resize =
  Signal.map Grid.Resize Window.dimensions
