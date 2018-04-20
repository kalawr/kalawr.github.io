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
        "/bucket-list/bucket-list.json"
        (Json.list itemDecoder)


itemDecoder =
    map2 Item
        (field "description" string)
        (field "achieved" bool)


type Msg
    = InitialResponse (Result Http.Error (List Item))
    | ToggleShowAchieved


update msg model =
    case msg of
        InitialResponse (Ok items) ->
            ( { model | items = items }
            , Cmd.none
            )

        InitialResponse (Err _) ->
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
        , ul []
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
