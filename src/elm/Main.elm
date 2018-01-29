port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Phoenix.Channel
import Phoenix.Socket
import Task


--- Model


type alias Model =
    { name : String
    , title : String
    , facility : String
    , feedCounts : Int
    , alertCounts : Int
    , snoozeCounts : Int
    , ackCounts : Int
    , listCounts : Int
    , filter : FilterState
    , patients : List Patient
    , postMessage : Maybe String
    , phxSocket : Phoenix.Socket.Socket SpotActions
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


type alias RequestMessage =
    { message : String }


initModel : Model
initModel =
    { name = "eht8620"
    , title = "Alerts List"
    , facility = "COCBR"
    , feedCounts = 0
    , alertCounts = 0
    , snoozeCounts = 0
    , ackCounts = 0
    , listCounts = 0
    , filter = AlertList
    , patients = []
    , postMessage = Nothing
    , phxSocket = initPhxSocket
    }



-- Url


apiUrl : String
apiUrl =
    "http://localhost:5555/alerts"


socketServer : String
socketServer =
    "ws://localhost:4000/socket/websocket"


initPhxSocket : Phoenix.Socket.Socket SpotActions
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "msg:alert" "spot:alert" GetPatientCard


joinChannel : Cmd SpotActions
joinChannel =
    Task.succeed JoinChannel
        |> Task.perform identity



-- Decoders/Encoders


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


requestMessage : JD.Decoder RequestMessage
requestMessage =
    decode RequestMessage
        |> Json.Decode.Pipeline.required "message" JD.string


encodePatientAction : Model -> String -> String -> Encode.Value
encodePatientAction model pid action =
    Encode.object
        [ ( "threefour", Encode.string model.name )
        , ( "facility", Encode.string model.facility )
        , ( "pid", Encode.string pid )
        , ( "action", Encode.string action )
        ]



-- Commands


getPatientCards : Cmd SpotActions
getPatientCards =
    JD.list patientDecoder
        |> Http.get apiUrl
        |> Http.send NewPatients


postPatientAction : Model -> String -> String -> Cmd SpotActions
postPatientAction model pid action =
    let
        url =
            "http://localhost:5555/api/alerts/actions"

        body =
            encodePatientAction model pid action
                |> Http.jsonBody

        request =
            Http.post url body requestMessage
    in
    Http.send PatientAction request



-- Update


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
    | PostAction Model String String
    | PatientAction (Result Http.Error RequestMessage)
    | Vitals String
    | GetPatientCard JD.Value
    | JoinChannel
    | PhoenixMsg (Phoenix.Socket.Msg SpotActions)


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
            ( { model | patients = List.map snoozePatient model.patients }, postPatientAction model pid "snooze" )

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
            ( { model | patients = List.map ackPatient model.patients }, postPatientAction model pid "ack" )

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
            ( { model | patients = List.map onListPatient model.patients }, postPatientAction model pid "onList" )

        RemoveFromList pid ->
            let
                removePatientList pat =
                    if pat.pid == pid then
                        { pat | state = "population" }
                    else
                        pat
            in
            ( { model | patients = List.map removePatientList model.patients }, postPatientAction model pid "removeFromList" )

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

        PostAction model pid action ->
            ( model, postPatientAction model pid action )

        PatientAction (Ok msg) ->
            let
                message =
                    "Patient Action" ++ toString msg
            in
            ( { model | postMessage = Just message }, Cmd.none )

        PatientAction (Err error) ->
            let
                message =
                    toString error
            in
            ( { model | postMessage = Just message }, Cmd.none )

        Vitals pid ->
            ( model, vitals pid )

        GetPatientCard raw ->
            case JD.decodeValue patientDecoder raw of
                Ok msg ->
                    ( { model
                        | patients = msg :: List.filter (\pat -> pat.pid /= msg.pid) model.patients
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        JoinChannel ->
            let
                channel =
                    Phoenix.Channel.init "spot:alert"

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )


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



-- Utils


tierValue : String -> String
tierValue tier =
    let
        tierCardValue =
            case tier of
                "3.0" ->
                    "Tier: 3"

                "2.0" ->
                    "Tier: 2"

                "1.0" ->
                    "Tier: 1"

                "0.0" ->
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
                "3.0" ->
                    "sp-tier__indicator sp-tier--3"

                "2.0" ->
                    "sp-tier__indicator sp-tier--2"

                "1.0" ->
                    "sp-tier__indicator sp-tier--1"

                _ ->
                    "sp-tier__indicator sp-tier--no-tier"
    in
    tierCardValue


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



-- View


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
                    , onClick (Vitals patient.pid)
                    ]
                    [ text "Vitals" ]
                ]
            ]
        ]


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
                    , onClick (Vitals patient.pid)
                    ]
                    [ text "Vitals" ]
                ]
            ]
        ]


viewPatientCardAlert : Patient -> Html SpotActions
viewPatientCardAlert patient =
    div
        [ class "sp-patient-card"
        , id ("sp-card-hi-" ++ patient.pid)
        , attribute "data-pid" patient.pid
        ]
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
                    , onClick (Vitals patient.pid)
                    ]
                    [ text "Vitals" ]
                ]
            ]
        ]


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
        , div [] [ text (toString model.postMessage) ]
        , div [ class "sp-alert__feed list" ]
            [ h2 [ class "sp-feed__header" ] [ text model.title ]
            , section [ class "main" ]
                [ ul [ id "svg" ]
                    (List.map (viewPatientCard model.filter) (patientState model))
                ]
            ]
        ]


subscriptions : Model -> Sub SpotActions
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg


main : Program Never Model SpotActions
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd SpotActions )
init =
    ( initModel
    , Cmd.batch
        [ getPatientCards
        , joinChannel
        ]
    )


port vitals : String -> Cmd msg
