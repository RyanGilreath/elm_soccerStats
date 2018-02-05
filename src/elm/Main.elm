module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Http exposing (..)
import LeagueTable exposing (..)


type alias Model =
    { leagueTable : Maybe LeagueTable
    , title : String
    }


init : ( Model, Cmd Msg )
init =
    ( { leagueTable = Nothing
      , title = "League Table"
      }
    , getResults
    )


soccerUrl : String
soccerUrl =
    "http://api.football-data.org/v1/competitions/445/leagueTable"


getResults : Cmd Msg
getResults =
    let
        url =
            soccerUrl

        request =
            Http.request
                { method = "GET"
                , url = url
                , body = emptyBody
                , headers =
                    [ Http.header "X-Auth-Token" "b38839047d9443db8fd79f2d48004857"
                    ]
                , expect = Http.expectJson leagueDataDecoder
                , timeout = Nothing
                , withCredentials = False
                }

        cmd =
            Http.send RequestData request
    in
    cmd


type Msg
    = RequestData (Result Error LeagueTable)
    | RecieveData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestData (Ok leagueTable) ->
            let
                _ =
                    Debug.log "Standing league" leagueTable
            in
            { model
                | leagueTable = Just leagueTable
            }
                ! []

        RequestData (Err err) ->
            let
                _ =
                    Debug.log "Error" (toString err)
            in
            model ! []

        RecieveData ->
            ( model, getResults )


viewTable : Model -> Html Msg
viewTable model =
    let
        leagueData =
            case model.leagueTable of
                Just data ->
                    data

                Nothing ->
                    { standings = []
                    }

        standingsCard =
            List.map
                (\s ->
                    li [ class "container" ]
                        [ div [ class "card grey lighten-4" ]
                            [ img
                                [ class "responsive-img"
                                , Attrs.src s.crestURI
                                , Attrs.style [ ( "width", "100px" ), ( "height", "100px" ) ]
                                ]
                                []
                            , h3 [] [ text (toString s.position) ]
                            , th [] [ text s.teamName ]
                            , td [ Attrs.style [ ( "text-align", "center" ) ] ]
                                [ text ("Points: " ++ toString s.points) ]
                            , td [ Attrs.style [ ( "text-align", "center" ) ] ]
                                [ text ("Wins: " ++ toString s.wins) ]
                            , td [ Attrs.style [ ( "text-align", "center" ) ] ]
                                [ text ("Losses: " ++ toString s.losses) ]
                            , td [ Attrs.style [ ( "text-align", "center" ) ] ]
                                [ text ("Draws: " ++ toString s.draws) ]
                            ]
                        ]
                )
                leagueData.standings
    in
    ul [] standingsCard


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Premier League Standings" ]
        , viewTable model
        ]



--test
-- subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
