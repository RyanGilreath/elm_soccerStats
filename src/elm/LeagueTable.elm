module LeagueTable exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias LeagueTable =
    { standings : List Standings
    }


type alias ExtraStandings =
    { goals : Int
    , goalsAgainst : Int
    , wins : Int
    , draws : Int
    , losses : Int
    }


type alias Standings =
    { position : Int
    , teamName : String
    , crestURI : String
    , playedGames : Int
    , points : Int
    , goals : Int
    , goalsAgainst : Int
    , goalDifference : Int
    , wins : Int
    , draws : Int
    , losses : Int
    , home : ExtraStandings
    , away : ExtraStandings
    }


leagueDataDecoder : Decode.Decoder LeagueTable
leagueDataDecoder =
    decode LeagueTable
        |> required "standing" (Decode.list standingsDecoder)


extraStandingDecoder : Decode.Decoder ExtraStandings
extraStandingDecoder =
    decode ExtraStandings
        |> required "goals" Decode.int
        |> required "goalsAgainst" Decode.int
        |> required "wins" Decode.int
        |> required "losses" Decode.int
        |> required "draws" Decode.int


standingsDecoder : Decode.Decoder Standings
standingsDecoder =
    decode Standings
        |> required "position" Decode.int
        |> required "teamName" Decode.string
        |> required "crestURI" Decode.string
        |> required "playedGames" Decode.int
        |> required "points" Decode.int
        |> required "goals" Decode.int
        |> required "goalsAgainst" Decode.int
        |> required "goalDifference" Decode.int
        |> required "wins" Decode.int
        |> required "draws" Decode.int
        |> required "losses" Decode.int
        |> required "home" extraStandingDecoder
        |> required "away" extraStandingDecoder
