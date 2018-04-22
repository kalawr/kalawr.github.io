module BucketList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (bool, field, list, map2, string)


type alias Model =
    { items : List Item
    , showAchieved : Bool
    }


type alias Item =
    { description : String
    , achieved : Bool
    }


init =
    ( { items = []
      , showAchieved = True
      }
    , Http.send InitialResponse initialRequest
    )


initialRequest =
    Http.get
        "https://sheets.googleapis.com/v4/spreadsheets/1Ehi5fNGVOfIR93FN5N4fNWfacvgSU17Vi3oFFmg17C8/values/Sheet1!A1:C99?key=AIzaSyBbS6tLJC7EKZBmeiAywSlzTOQ-selKBns"
        googleSheetResponseDecoder


itemDecoder =
    map2 Item
        (field "description" string)
        (field "achieved" bool)

googleSheetResponseDecoder =
    (Json.field "values" (Json.list googleSheetItemDecoder))

googleSheetItemDecoder =
    Json.oneOf [googleSheetCheckedItemDecoder, googleSheetUncheckedItemDecoder]

googleSheetCheckedItemDecoder =
    map2 Item
        (Json.index 0 string)
        (Json.index 1 googleSheetCheckboxDecoder)

googleSheetUncheckedItemDecoder =
    map2 Item
        (Json.index 0 string)
        (Json.succeed False)


googleSheetCheckboxDecoder =
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
    = InitialResponse (Result Http.Error (List Item))
    | ToggleShowAchieved


update msg model =
    case msg of
        InitialResponse (Ok items) ->
            ( { model | items = items }
            , Cmd.none
            )

        InitialResponse (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            ( model
            , Cmd.none
            )

        ToggleShowAchieved ->
            ( { model | showAchieved = not model.showAchieved }
            , Cmd.none
            )


view model =
    let
        list =
            if model.showAchieved then
                model.items
            else
                model.items
                    |> List.filter (not << .achieved)
    in
    div []
        [ h1 [] [ text "Bucket List" ]
        , label []
            [ input [ type_ "checkbox", checked model.showAchieved, onClick ToggleShowAchieved ] []
            , text "Show achieved items"
            ]
        , ul [class "bucket"]
            (list |> List.map item)
        ]


item i =
    li [ classList [ ( "done", i.achieved ) ] ]
        [ text i.description
        ]


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
