module BucketList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Random
import Random.List
import RemoteData exposing (RemoteData(..), WebData)
import Task exposing (Task)
import Time


type alias Model =
    { items : WebData (List Item)
    , showDone : Bool
    }


type alias Item =
    { description : String
    , done : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { items = Loading
      , showDone = True
      }
    , initialRequest
        |> Http.toTask
        |> Task.andThen shuffle
        |> RemoteData.asCmd
        |> Cmd.map InitialResponse
    )


shuffle : List Item -> Task e (List Item)
shuffle items =
    Time.now
        |> Task.map
            (\time ->
                time
                    |> round
                    |> Random.initialSeed
                    |> Random.step (Random.List.shuffle items)
                    |> Tuple.first
            )


sheetId : String
sheetId =
    "1Ehi5fNGVOfIR93FN5N4fNWfacvgSU17Vi3oFFmg17C8"


sheetUri : String
sheetUri =
    String.concat
        [ "https://sheets.googleapis.com/v4/spreadsheets/"
        , sheetId
        , "/values/Sheet1!A1:C99?key=AIzaSyBbS6tLJC7EKZBmeiAywSlzTOQ-selKBns"
        ]


initialRequest : Http.Request (List Item)
initialRequest =
    Http.get
        sheetUri
        decoder


decoder : Json.Decoder (List Item)
decoder =
    Json.field "values" (Json.list itemDecoder)


itemDecoder : Json.Decoder Item
itemDecoder =
    Json.oneOf
        [ checkedItemDecoder
        , uncheckedItemDecoder
        ]


checkedItemDecoder : Json.Decoder Item
checkedItemDecoder =
    Json.map2 Item
        (Json.index 0 Json.string)
        (Json.index 1 checkboxDecoder)


uncheckedItemDecoder : Json.Decoder Item
uncheckedItemDecoder =
    Json.map2 Item
        (Json.index 0 Json.string)
        (Json.succeed False)


checkboxDecoder : Json.Decoder Bool
checkboxDecoder =
    Json.string
        |> Json.andThen
            (\string ->
                case string of
                    "TRUE" ->
                        Json.succeed True

                    _ ->
                        Json.succeed False
            )


type Msg
    = InitialResponse (WebData (List Item))
    | ToggleShowDone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialResponse response ->
            ( { model | items = response }
            , Cmd.none
            )

        ToggleShowDone ->
            ( { model | showDone = not model.showDone }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Bucket List" ]
        , description model.items
        , label [ class "bucket-list-toggle" ]
            [ input [ type_ "checkbox", checked model.showDone, onClick ToggleShowDone ] []
            , text "Show achieved items"
            ]
        , list model
        ]


description : WebData (List Item) -> Html Msg
description webdata =
    case RemoteData.toMaybe webdata of
        Just items ->
            let
                n =
                    List.length items
            in
            p []
                [ text (toString n)
                , text " things to do before I die."
                ]

        Nothing ->
            text ""


list : Model -> Html Msg
list model =
    case model.items of
        Success items ->
            let
                list =
                    if model.showDone then
                        items
                    else
                        items
                            |> List.filter (not << .done)
            in
            ul [ class "bucket" ]
                (list |> List.map item)

        Loading ->
            text "Loading..."

        Failure _ ->
            text "Error fetching bucket list items"

        NotAsked ->
            text ""


item : Item -> Html Msg
item i =
    li [ classList [ ( "done", i.done ) ] ]
        [ if i.done then
            span [ class "icon-check" ] []
          else
            text ""
        , span [ class "bucket-list-description" ] [ text i.description ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
