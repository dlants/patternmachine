module PatternMachine exposing (..)

import Platform.Cmd as Cmd
import Window
import Array
import Navigation
import String
import Matrix
import Task
import Html exposing (..)
import Html.App as App
import Grid


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- URL PARSERS


toUrl : Matrix.Matrix Int -> String
toUrl grid =
    let
        hash =
            Matrix.toIndexedArray grid
                |> Array.map (\( _, val ) -> toString val)
                |> Array.toList
                |> String.join ""
    in
        "#/" ++ hash


url2intlist : String -> List Int
url2intlist url =
    (String.dropLeft 2 url)
        |> String.toList
        |> List.map String.fromChar
        |> List.map String.toInt
        |> List.map (Result.withDefault 1)


padToTen : List Int -> List Int
padToTen list =
    List.repeat (10 - (List.length list)) 1
        |> List.append list


groupByTens : List Int -> List (List Int)
groupByTens list =
    [0..9]
        |> List.map
            (\idx ->
                List.drop (idx * 10) list
                    |> List.take 10
                    |> padToTen
            )


fromUrl : String -> Result String (Matrix.Matrix Int)
fromUrl url =
    let
        results =
            url2intlist url
    in
        groupByTens results
            |> Matrix.fromList
            |> Result.fromMaybe "error parsing hash"


urlParser : Navigation.Parser (Result String (Matrix.Matrix Int))
urlParser =
    Navigation.makeParser (fromUrl << .hash)


urlUpdate : Result String (Matrix.Matrix Int) -> Grid.Model -> ( Grid.Model, Cmd Msg )
urlUpdate result model =
    case result of
        Ok newMatrix ->
            ( { model | matrix = newMatrix }, Cmd.none )

        Err a ->
            ( model, Navigation.modifyUrl (toUrl model.matrix) )


init : Result String (Matrix.Matrix Int) -> ( Grid.Model, Cmd Msg )
init result =
    let
        model =
            case result of
                Ok matrix ->
                    { matrix = matrix
                    , window = { width = 100, height = 100 }
                    }

                Err _ ->
                    { matrix = (Matrix.repeat 10 10 1)
                    , window = { width = 100, height = 100 }
                    }
    in
        ( model, windowCmd )


type Msg
    = NoOp
    | Resize Window.Size
    | GridMsg Grid.Msg


onError : a -> Msg
onError x =
    NoOp


onSuccess : Window.Size -> Msg
onSuccess size =
    Resize size


windowCmd : Cmd Msg
windowCmd =
    Task.perform onError onSuccess Window.size


update : Msg -> Grid.Model -> ( Grid.Model, Cmd Msg )
update msg model =
    let
        model =
            case msg of
                NoOp ->
                    model

                Resize newSize ->
                    { model | window = newSize }

                GridMsg gridMsg ->
                    Grid.update gridMsg model
    in
        ( model, Navigation.modifyUrl (toUrl model.matrix) )


subscriptions : Grid.Model -> Sub Msg
subscriptions model =
    Window.resizes Resize


view : Grid.Model -> Html Msg
view model =
    App.map GridMsg (Grid.view model)
