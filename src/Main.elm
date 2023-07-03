module Main exposing (main)

import Browser
import Fireworks exposing (Color(..), Firework, fireworkAt, fireworkView)
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import List.Extra exposing (setAt)
import Maybe.Extra
import Particle.System
import Process
import Random
import Random.Extra
import Random.Float exposing (normal)
import Task
import VitePluginHelper



-- Msg


type Role
    = Player1
    | Player2


type Msg
    = SetScore Role (Maybe Int) (Maybe Int)
    | ResetGame
    | SetCurrentThrow Role (Maybe Int)
    | SetShowResetModal Bool
    | Detonate
    | ParticleMsg (Particle.System.Msg Firework)



-- Model


type alias Player =
    { throws : List (Maybe Int)
    , currentThrow : Maybe Int
    }


type alias Model =
    { player1 : Player
    , player2 : Player
    , showResetModal : Bool
    , fireworks : Particle.System.System Firework
    }


newPlayer : Player
newPlayer =
    { throws =
        [ Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        ]
    , currentThrow = Nothing
    }


newModel : Model
newModel =
    { player1 = newPlayer
    , player2 = newPlayer
    , showResetModal = False
    , fireworks = Particle.System.init <| Random.initialSeed 0
    }



-- Main


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \model -> Particle.System.sub [] ParticleMsg model.fireworks }



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( newModel, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetGame ->
            ( newModel, Cmd.none )

        SetCurrentThrow player throw ->
            case player of
                Player1 ->
                    ( { model | player1 = { currentThrow = throw, throws = model.player1.throws } }, Cmd.none )

                Player2 ->
                    ( { model | player2 = { currentThrow = throw, throws = model.player2.throws } }, Cmd.none )

        SetShowResetModal show ->
            ( { model | showResetModal = show }, Cmd.none )

        Detonate ->
            let
                newFireworks =
                    Particle.System.burst
                        (Random.Extra.andThen3 fireworkAt
                            (Random.uniform Red [ Green, Blue ])
                            (normal 500 500)
                            (normal 500 500)
                        )
                        model.fireworks
            in
            ( { model | fireworks = newFireworks }, Cmd.none )

        ParticleMsg inner ->
            ( { model | fireworks = Particle.System.update inner model.fireworks }, Cmd.none )

        SetScore player throw score ->
            let
                playerThrows =
                    case player of
                        Player1 ->
                            model.player1.throws

                        Player2 ->
                            model.player2.throws

                throwNumber throws =
                    case throw of
                        Nothing ->
                            throws
                                |> List.Extra.findIndex
                                    (\t ->
                                        case t of
                                            Nothing ->
                                                True

                                            _ ->
                                                False
                                    )

                        _ ->
                            throw

                setThrows throws =
                    case throwNumber throws of
                        Just t ->
                            setAt t score throws

                        _ ->
                            throws

                perfectGame =
                    playerThrows
                        |> setThrows
                        |> List.map (\t -> Maybe.withDefault 0 t)
                        |> List.foldl (\t acc -> t + acc) 0
                        |> (\total -> total == 25)

                cmd =
                    if perfectGame then
                        [ delay 0 Detonate
                        , delay 1000 Detonate
                        , delay 2000 Detonate
                        , delay 3000 Detonate
                        , delay 4000 Detonate
                        , delay 5000 Detonate
                        , delay 6000 Detonate
                        , delay 7000 Detonate
                        ]
                            |> Cmd.batch

                    else
                        Cmd.none
            in
            case player of
                Player1 ->
                    ( { model | player1 = { throws = setThrows model.player1.throws, currentThrow = Nothing } }, cmd )

                Player2 ->
                    ( { model | player2 = { throws = setThrows model.player2.throws, currentThrow = Nothing } }, cmd )



-- View


view : Model -> Html Msg
view model =
    div
        [ class "flex flex-col justify-center items-center min-w-screen min-h-screen bg-thunder-900 gap-12"
        , style "background-image" ("url('" ++ VitePluginHelper.asset "/src/assets/chains.svg" ++ "')")
        ]
        [ div [ class "flex items-center justify-center gap-[126px]" ]
            [ game model Player1
            , game model Player2
            ]
        , button
            [ onClick <| SetShowResetModal True, class "w-[256px] h-12 bg-red-600 font-bold text-white text-[24px]" ]
            [ text "Reset Match" ]
        , Particle.System.view fireworkView [ style "width" "100vw", style "height" "100vh", style "position" "fixed", style "pointer-events" "none", style "top" "0", style "left" "0" ] model.fireworks
        , resetModal model
        ]


game : Model -> Role -> Html Msg
game model player =
    let
        ( playerDisplay, throws, currentThrow ) =
            case player of
                Player1 ->
                    ( "Player 1", model.player1.throws, model.player1.currentThrow )

                Player2 ->
                    ( "Player 2", model.player2.throws, model.player2.currentThrow )

        roundOver =
            case List.Extra.find (\t -> Maybe.Extra.isNothing t) throws of
                Nothing ->
                    True

                _ ->
                    False

        totalScore =
            List.foldl
                (\throw acc ->
                    case throw of
                        Just s ->
                            s + acc

                        _ ->
                            acc
                )
                0
                throws
    in
    div [ class "flex flex-col justify-center items-center gap-6" ]
        [ div [ class "flex flex-col justify-center items-center gap-4" ]
            -- Header
            [ div
                [ style "background-image"
                    ("url('"
                        ++ VitePluginHelper.asset
                            "/src/assets/planck.svg"
                        ++ "')"
                    )
                , style "background-size" "cover"
                , class "flex justify-center items-center h-12 w-[218px]"
                ]
                [ h1 [ class "font-bold text-[24px] text-white" ] [ text playerDisplay ] ]

            -- Total Score
            , p [ class "font-bold text-[36px] text-white" ] [ text (String.fromInt totalScore) ]

            -- Individual Throws
            , throwsDisplay player throws currentThrow
            ]

        -- Score Buttons
        , div [ class "flex flex-col gap-4 w-full" ]
            ([ 0, 1, 2, 3, 4, 5 ]
                |> List.map (\p -> scoreButton player p currentThrow (roundOver && Maybe.Extra.isNothing currentThrow))
            )
        ]


scoreButton : Role -> Int -> Maybe Int -> Bool -> Html Msg
scoreButton player points currentThrow isDisabled =
    let
        style =
            if not isDisabled then
                "w-full h-10 bg-hatchets-green-600 font-bold text-white text-[18px]"

            else
                "w-full h-10 bg-hatchets-green-600/50 font-bold text-thunder-50 text-[18px] cursor-not-allowed"
    in
    button
        [ onClick (SetScore player currentThrow (Just points))
        , class style
        , disabled isDisabled
        ]
        [ text <| String.fromInt points ]


throwsDisplay : Role -> List (Maybe Int) -> Maybe Int -> Html Msg
throwsDisplay player throws currentThrow =
    let
        singleThrow throw index =
            let
                isCurrentThrow =
                    case currentThrow of
                        Just t ->
                            t == index

                        _ ->
                            let
                                t =
                                    List.Extra.findIndex (\elem -> Maybe.Extra.isNothing elem) throws
                            in
                            case t of
                                Just i ->
                                    i == index

                                _ ->
                                    False

                border =
                    if isCurrentThrow then
                        " border border-hatchets-green-300"

                    else
                        ""
            in
            case throw of
                Just s ->
                    button [ onClick <| SetCurrentThrow player (Just index), class <| "flex items-center justify-center h-12 w-12 bg-thunder-500 text-white text-[24px] font-bold" ++ border ] [ text <| String.fromInt s ]

                _ ->
                    button [ onClick <| SetCurrentThrow player (Just index), class <| "flex items-center justify-center h-12 w-12 bg-thunder-500/40 text-thunder-600 text-[24px] font-bold" ++ border ] [ text "0" ]
    in
    div [ class "flex items-center gap-8" ]
        (List.indexedMap
            (\i t -> singleThrow t i)
            throws
        )


resetModal : Model -> Html Msg
resetModal model =
    if model.showResetModal then
        div [ class "fixed flex items-center justify-center w-screen h-screen inset-0 bg-black/50" ]
            [ div [ class "flex flex-col justify-center items-center gap-6 bg-thunder-50 p-8" ]
                [ h2 [ class "text-2xl font-bold" ] [ text "Reset Match" ]
                , p [] [ text "Are you sure you want to reset the match?" ]
                , div [ class "flex items-center justify-between w-full" ]
                    [ button [ onClick <| SetShowResetModal False, class "px-4 py-3 bg-thunder-400 text-white" ] [ text "Back" ]
                    , button [ onClick ResetGame, class "px-4 py-3 bg-red-600 text-white" ] [ text "Reset" ]
                    ]
                ]
            ]

    else
        div [ class "hidden" ] []



-- Utility


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
