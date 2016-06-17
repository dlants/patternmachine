module PatternMachine exposing (..)

import Platform.Cmd as Cmd
import Window
import Task
import Html exposing (..)
import Html.App as App
import Grid

init =
    (Grid.init {width=100, height=100}, windowCmd)

type Msg =
  NoOp
  | Resize Window.Size
  | GridMsg Grid.Msg

onError x =
  NoOp

onSuccess: Window.Size -> Msg
onSuccess size = Resize size


windowCmd =
  Task.perform onError onSuccess Window.size

update: Msg -> Grid.Model -> (Grid.Model, Cmd Msg)
update msg model =
  let model = case msg of
    NoOp ->
      model
    Resize newSize ->
      {model | window = newSize}
    GridMsg gridMsg ->
      Grid.update gridMsg model
  in (model, windowCmd)

subscriptions: Grid.Model -> Sub Msg
subscriptions model =
  Window.resizes Resize

view: Grid.Model -> Html Msg
view model =
  App.map GridMsg (Grid.view model)

main = App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
