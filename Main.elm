module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias Model =
    { name : String
    , title : String
    , facility : String
    , feedCounts : Int
    , filter : FilterState
    , patients : List Patient
    }


type alias Patient =
    { pid : String
    , name : String
    , reason_for_visit : String
    , readmit : Bool
    , gender : String
    , facility : String
    , alert : String
    , age : String
    , accid : String
    , unit : String
    , room : String
    , state : String
    , tier : String
    }


initModel : Model
initModel =
    { name = "eht8620"
    , title = "Alerts List"
    , facility = "COCBR"
    , feedCounts = 0
    , filter = AlertList
    , patients = []
    }



-- initPatients : List Patient
-- initPatients =
--     [ Patient "COCBR12345" "Jane Doe" "Warm-Leg" "alert" "detection" True False False
--     , Patient "COCBR155235" "Ryan Smith" "Vomit" "alert" "detection" True False False
--     , Patient "COCBR763245" "John Doe" "cold" "alert" "positveScreen" True False False
--     ]
--


apiUrl : String
apiUrl =
    "http://localhost:4000/alerts"


getPatientCards : Cmd SpotActions
getPatientCards =
    JD.list patientDecoder
        |> Http.get apiUrl
        |> Http.send NewPatients


patientDecoder : JD.Decoder Patient
patientDecoder =
    decode Patient
        |> Json.Decode.Pipeline.required "pid" JD.string
        |> Json.Decode.Pipeline.required "name" JD.string
        |> Json.Decode.Pipeline.required "reason_for_visit" JD.string
        |> Json.Decode.Pipeline.required "readmit" JD.bool
        |> Json.Decode.Pipeline.required "gender" JD.string
        |> Json.Decode.Pipeline.required "facility" JD.string
        |> Json.Decode.Pipeline.required "alert" JD.string
        |> Json.Decode.Pipeline.required "age" JD.string
        |> Json.Decode.Pipeline.required "accid" JD.string
        |> Json.Decode.Pipeline.required "unit" JD.string
        |> Json.Decode.Pipeline.required "room" JD.string
        |> Json.Decode.Pipeline.required "state" JD.string
        |> Json.Decode.Pipeline.required "tier" JD.string


type SpotActions
    = SnoozeFeed
    | AlertFeed
    | AckFeed
    | ListFeed
    | Snooze String
    | Ack String
    | OnList String
    | RemoveFromList String
    | RemoveFromSnooze String
    | Filter FilterState
    | NewPatients (Result Http.Error (List Patient))


update : SpotActions -> Model -> ( Model, Cmd SpotActions )
update msg model =
    case msg of
        SnoozeFeed ->
            ( { model
                | title = "Snooze List"
              }
            , Cmd.none
            )

        AlertFeed ->
            ( { model
                | title = "Alert List"
              }
            , Cmd.none
            )

        AckFeed ->
            ( { model
                | title = "Acknowledged List"
              }
            , Cmd.none
            )

        ListFeed ->
            ( { model
                | title = "Care List"
              }
            , Cmd.none
            )

        Snooze pid ->
            let
                snoozePatient pat =
                    if pat.pid == pid then
                        { pat
                            | state = "snooze"
                        }
                    else
                        pat
            in
            ( { model | patients = List.map snoozePatient model.patients }, Cmd.none )

        Ack pid ->
            let
                ackPatient pat =
                    if pat.pid == pid then
                        { pat
                            | state = "ack"
                        }
                    else
                        pat
            in
            ( { model | patients = List.map ackPatient model.patients }, Cmd.none )

        OnList pid ->
            let
                onListPatient pat =
                    if pat.pid == pid then
                        { pat
                            | state = "onList"
                        }
                    else
                        pat
            in
            ( { model | patients = List.map onListPatient model.patients }, Cmd.none )

        RemoveFromList pid ->
            let
                removePatientList pat =
                    if pat.pid == pid then
                        { pat | state = "population" }
                    else
                        pat
            in
            ( { model | patients = List.map removePatientList model.patients }, Cmd.none )

        RemoveFromSnooze pid ->
            let
                removePatientSnooze pat =
                    if pat.pid == pid then
                        { pat | state = "alert" }
                    else
                        pat
            in
            ( { model | patients = List.map removePatientSnooze model.patients }, Cmd.none )

        Filter filterState ->
            ( { model | filter = filterState }, Cmd.none )

        NewPatients (Ok patientCards) ->
            ( { model | patients = patientCards }, Cmd.none )

        NewPatients (Err error) ->
            let
                _ =
                    Debug.log "NOPE" error
            in
            ( model, Cmd.none )


type FilterState
    = AlertList
    | SnoozeList
    | AckList
    | CareList


patientState : Model -> List Patient
patientState model =
    let
        filterPatients =
            case model.filter of
                AlertList ->
                    \patient -> patient.state == "alert"

                SnoozeList ->
                    \patient -> patient.state == "snooze"

                AckList ->
                    \patient -> patient.state == "ack"

                CareList ->
                    \patient -> patient.state == "onList"
    in
    List.filter filterPatients model.patients


tierValue : String -> String
tierValue tier =
    let
        tierCardValue =
            case tier of
                "3" ->
                    "Tier: 3"

                "2" ->
                    "Tier: 2"

                "1" ->
                    "Tier: 1"

                "0" ->
                    "Tier: 0"

                _ ->
                    "Sepsis Screen required"
    in
    tierCardValue


tierClass : String -> String
tierClass tier =
    let
        tierCardValue =
            case tier of
                "3" ->
                    "sp-tier__indicator sp-tier--3"

                "2" ->
                    "sp-tier__indicator sp-tier--2"

                "1" ->
                    "sp-tier__indicator sp-tier--1"

                _ ->
                    "sp-tier__indicator sp-tier--no-tier"
    in
    tierCardValue


viewPatientCard : FilterState -> Patient -> Html SpotActions
viewPatientCard filterState =
    let
        htmlView =
            case filterState of
                AlertList ->
                    viewPatientCardAlert

                SnoozeList ->
                    viewPatientCardSnooze

                AckList ->
                    viewPatientCardAck

                CareList ->
                    viewPatientCardList
    in
    htmlView


viewPatientCardSnooze : Patient -> Html SpotActions
viewPatientCardSnooze patient =
    div [ class "sp-patient-card" ]
        [ section [ class "sp-patient__alert", style [ ( "text-align", "center" ) ] ]
            [ header [ class "sp-alert__reason" ]
                [ h3
                    [ class "js-name"
                    , style [ ( "text-decoration", "none" ), ( "letter-spacing", "1.5px" ) ]
                    ]
                    [ text patient.name ]
                ]
            , section [ class "sp-patient--actions" ]
                [ button
                    [ class "sp-btn sp-btn--success list-background white js-action"
                    , onClick (OnList patient.pid)
                    ]
                    [ text "Add to List" ]
                , button
                    [ class "sp-btn sp-btn--danger list-background white js-action"
                    , onClick (RemoveFromSnooze patient.pid)
                    ]
                    [ text "Remove" ]
                , button
                    [ class "sp-btn sp-btn--primary snooze-background white js-vitals"
                    ]
                    [ text "Vitals" ]
                ]
            ]
        ]


viewPatientCardAck : Patient -> Html SpotActions
viewPatientCardAck patient =
    h2 [] [ text patient.name ]


viewPatientCardList : Patient -> Html SpotActions
viewPatientCardList patient =
    div [ class "sp-patient-card" ]
        [ section [ class (tierClass patient.tier) ]
            [ header []
                [ section []
                    [ h3 [ class "js-tier" ] [ text (tierValue patient.tier) ] ]
                ]
            ]
        , section [ class "sp-patient__alert", style [ ( "text-align", "center" ) ] ]
            [ header [ class "sp-alert__reason" ]
                [ h3
                    [ class "js-name"
                    , style [ ( "text-decoration", "none" ), ( "letter-spacing", "1.5px" ) ]
                    ]
                    [ text patient.name ]
                , span [ class "js-cause" ]
                    [ p [ class "sp-dt-reason" ] [ text patient.alert ] ]
                ]
            , main_ [ class "sp-patient__details" ]
                [ div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "ACCID" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.accid ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "AGE" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.age ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "UNIT" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.unit ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "ROOM" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.room ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "Reason For Visit" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.reason_for_visit ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "Gender" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.gender ] ]
                    ]
                ]
            , section [ class "sp-patient--actions" ]
                [ button
                    [ class "sp-btn sp-btn--danger list-background white js-action"
                    , onClick (RemoveFromList patient.pid)
                    ]
                    [ text "Remove" ]
                , button
                    [ class "sp-btn sp-btn--primary snooze-background white js-vitals"
                    ]
                    [ text "Vitals" ]
                ]
            ]
        ]


viewPatientCardAlert : Patient -> Html SpotActions
viewPatientCardAlert patient =
    div [ class "sp-patient-card" ]
        [ section [ class (tierClass patient.tier) ]
            [ header []
                [ section []
                    [ h3 [ class "js-tier" ] [ text (tierValue patient.tier) ] ]
                ]
            ]
        , section [ class "sp-patient__alert", style [ ( "text-align", "center" ) ] ]
            [ header [ class "sp-alert__reason" ]
                [ h3
                    [ class "js-name"
                    , style [ ( "text-decoration", "none" ), ( "letter-spacing", "1.5px" ) ]
                    ]
                    [ text patient.name ]
                , span [ class "js-cause" ]
                    [ p [ class "sp-dt-reason" ] [ text patient.alert ] ]
                ]
            , main_ [ class "sp-patient__details" ]
                [ div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "ACCID" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.accid ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "AGE" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.age ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "UNIT" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.unit ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "ROOM" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.room ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "Reason For Visit" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.reason_for_visit ] ]
                    ]
                , div [ class "sp-patient__elem" ]
                    [ span [ class "sp-elem__label" ] [ text "Gender" ]
                    , div [ class "sp-elem" ]
                        [ h4 [ class "js-accid" ] [ text patient.gender ] ]
                    ]
                ]
            , section [ class "sp-patient--actions" ]
                [ button
                    [ class "sp-btn sp-btn--success list-background white js-action"
                    , onClick (OnList patient.pid)
                    ]
                    [ text "Add to List" ]
                , button
                    [ class "sp-btn sp-btn--secondary ack-background white js-action"
                    , onClick (Ack patient.pid)
                    ]
                    [ text "Ack" ]
                , button
                    [ class "sp-btn sp-btn--warning snooze-background white js-action"
                    , onClick (Snooze patient.pid)
                    ]
                    [ text "Snooze" ]
                , button
                    [ class "sp-btn sp-btn--primary snooze-background white js-vitals"
                    ]
                    [ text "Vitals" ]
                ]
            ]
        ]


filterPatientsFeed : Model -> FilterState -> Html SpotActions
filterPatientsFeed model filterState =
    let
        headerTitle =
            case filterState of
                AlertList ->
                    "Alerts"

                SnoozeList ->
                    "Snoozed"

                AckList ->
                    "Acknowledged"

                CareList ->
                    "List"
    in
    a [ href "#", class "center-header", onClick (Filter filterState) ]
        [ text headerTitle ]


stateCounts : String -> List Patient -> Html SpotActions
stateCounts state patientList =
    let
        patientCount =
            case state of
                "alert" ->
                    List.length (List.filter (\patient -> patient.state == "alert") patientList)

                "snooze" ->
                    List.length (List.filter (\patient -> patient.state == "snooze") patientList)

                "ack" ->
                    List.length (List.filter (\patient -> patient.state == "ack") patientList)

                "onList" ->
                    List.length (List.filter (\patient -> patient.state == "onList") patientList)

                _ ->
                    0
    in
    label [] [ patientCount |> toString |> text ]


view : Model -> Html SpotActions
view model =
    div [ class "container" ]
        [ ul [ class "sp-stats" ]
            [ li [ class "center-header", onClick AlertFeed ]
                [ filterPatientsFeed model AlertList
                , stateCounts "alert" model.patients
                ]
            , li [ class "center-header", onClick SnoozeFeed ]
                [ filterPatientsFeed model SnoozeList
                , stateCounts "snooze" model.patients
                ]
            , li [ class "center-header", onClick AckFeed ]
                [ filterPatientsFeed model AckList
                , stateCounts "ack" model.patients
                ]
            , li [ class "center-header", onClick ListFeed ]
                [ filterPatientsFeed model CareList
                , stateCounts "onList" model.patients
                ]
            ]
        , div [ class "sp-alert__feed list" ]
            [ h2 [ class "sp-feed__header" ] [ text model.title ]
            , section [ class "main" ]
                [ ul [ class "" ]
                    (List.map (viewPatientCard model.filter) (patientState model))
                ]
            ]
        ]


main : Program Never Model SpotActions
main =
    Html.program
        { init = ( initModel, getPatientCards )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
