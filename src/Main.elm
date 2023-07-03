module Main exposing (main)

import Browser
import Fireworks exposing (Color(..), fireworkAt, fireworkView)
import Game exposing (game)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List.Extra exposing (setAt)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Particle.System
import Random
import Random.Extra
import Random.Float exposing (normal)
import ResetModal exposing (resetModal)
import VitePluginHelper


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \model -> Particle.System.sub [] ParticleMsg model.fireworks }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetGame ->
            ( Model.init, Cmd.none )

        SetCurrentThrow player throw ->
            case player of
                Msg.Player1 ->
                    ( { model | player1 = { currentThrow = throw, throws = model.player1.throws } }, Cmd.none )

                Msg.Player2 ->
                    ( { model | player2 = { currentThrow = throw, throws = model.player2.throws } }, Cmd.none )

        SetShowResetModal show ->
            ( { model | showResetModal = show }, Cmd.none )

        Detonate ->
            let
                newFireworks =
                    Particle.System.burst
                        (Random.Extra.andThen3 fireworkAt
                            (Random.uniform Red [ Green, Blue ])
                            (normal 300 100)
                            (normal 300 100)
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
                        Msg.Player1 ->
                            model.player1.throws

                        Msg.Player2 ->
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

                fireworks =
                    if perfectGame then
                        Particle.System.burst
                            (Random.Extra.andThen3 fireworkAt
                                (Random.uniform Red [ Green, Blue ])
                                (normal 300 100)
                                (normal 300 100)
                            )
                            model.fireworks

                    else
                        model.fireworks
            in
            case player of
                Msg.Player1 ->
                    ( { model | player1 = { throws = setThrows model.player1.throws, currentThrow = Nothing }, fireworks = fireworks }, Cmd.none )

                Msg.Player2 ->
                    ( { model | player2 = { throws = setThrows model.player2.throws, currentThrow = Nothing }, fireworks = fireworks }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "flex flex-col justify-center items-center min-w-screen min-h-screen bg-thunder-900 gap-12"
        , style "background-image" ("url('" ++ VitePluginHelper.asset "/src/assets/chains.svg" ++ "')")
        ]
        [ div [ class "flex items-center justify-center gap-[126px]" ]
            [ game model Msg.Player1
            , game model Msg.Player2
            ]
        , button
            [ onClick <| SetShowResetModal True, class "w-[256px] h-12 bg-red-600 font-bold text-white text-[24px]" ]
            [ text "Reset Match" ]
        , Particle.System.view fireworkView [ style "width" "100vw", style "height" "100vh", style "position" "fixed", style "pointer-events" "none", style "top" "0", style "left" "0" ] model.fireworks
        , resetModal model
        ]
