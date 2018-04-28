module BucketList exposing (..)

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Random
import Random.List
import RemoteData exposing (RemoteData(..), WebData)
import Task exposing (Task)
import Time exposing (Time)


type alias Model =
    { items : WebData (List Item)
    , showDone : Bool
    , now : Maybe Time
    }


type alias Item =
    { description : String
    , done : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { items = Loading
      , showDone = True
      , now = Nothing
      }
    , Cmd.batch
        [ initialRequest
            |> Http.toTask
            |> Task.andThen shuffle
            |> RemoteData.asCmd
            |> Cmd.map InitialResponse
        , Task.perform NewTime Time.now
        ]
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


googleApiKey : String
googleApiKey =
    "AIzaSyBbS6tLJC7EKZBmeiAywSlzTOQ-selKBns"


sheetUri : String
sheetUri =
    String.concat
        [ "https://sheets.googleapis.com/v4/spreadsheets/"
        , sheetId
        , "/values/Sheet1!A1:C99?key="
        , googleApiKey
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
    | NewTime Time


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

        NewTime time ->
            ( { model | now = Just time }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.items of
        Success items ->
            div []
                [ h1 [] [ text "Bucket List" ]
                , description items
                , progress items model.now
                , toggle model.showDone
                , list items model
                ]

        Loading ->
            text "Loading..."

        Failure _ ->
            text "Error fetching bucket list items"

        NotAsked ->
            text ""


description : List Item -> Html Msg
description items =
    let
        n =
            List.length items
    in
    p []
        [ text (toString n)
        , text " things to do before I die."
        ]


progress : List Item -> Maybe Time -> Html Msg
progress items now =
    Maybe.map
        (\time ->
            let
                life =
                    lifeProgress time
                        |> roundProgress
                        |> toString

                list =
                    listProgress items
                        |> roundProgress
                        |> toString
            in
            div []
                [ p []
                    [ text "List progress "
                    , meter [ value list ] []
                    ]
                , p []
                    [ text "Life progress "
                    , meter [ value life ] []
                    ]
                ]
        )
        now
        |> Maybe.withDefault (text "")


toggle : Bool -> Html Msg
toggle showDone =
    label [ class "bucket-list-toggle" ]
        [ input [ type_ "checkbox", checked showDone, onClick ToggleShowDone ] []
        , text "Show achieved items"
        ]


list : List Item -> Model -> Html Msg
list items model =
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


item : Item -> Html Msg
item i =
    li [ classList [ ( "done", i.done ) ] ]
        [ span [ class "bucket-list-description" ] [ text i.description ]
        ]


{-| Source: `https://www.indexmundi.com/belarus/life_expectancy_at_birth.html`
-}
belarusianMaleLifeExpectancy : Float
belarusianMaleLifeExpectancy =
    67.5 * Time.hour * 24 * 365


birthday : Time
birthday =
    Date.fromString "August 21, 1992"
        |> Result.map Date.toTime
        |> Result.withDefault 0


listProgress : List Item -> Float
listProgress list =
    let
        completed =
            list
                |> List.filter .done
                |> List.length
                |> toFloat

        all =
            list
                |> List.length
                |> toFloat
    in
    completed / all


lifeProgress : Time -> Float
lifeProgress now =
    let
        estimatedAgeOfDeath =
            birthday + belarusianMaleLifeExpectancy
    in
    (now - birthday) / estimatedAgeOfDeath


roundProgress : Float -> Float
roundProgress n =
    n
        |> (*) 100
        |> round
        |> toFloat
        |> (\a -> (/) a 100)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
