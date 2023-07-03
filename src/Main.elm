module Main exposing (main)

import Browser
import Game exposing (game)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List.Extra exposing (setAt)
import Model exposing (Model)
import Msg exposing (Msg(..))
import ResetModal exposing (resetModal)
import VitePluginHelper


main : Program () Model Msg
main =
    Browser.sandbox { init = Model.init, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ResetGame ->
            Model.init

        SetCurrentThrow player throw ->
            case player of
                Msg.Player1 ->
                    { model | player1 = { currentThrow = throw, throws = model.player1.throws } }

                Msg.Player2 ->
                    { model | player2 = { currentThrow = throw, throws = model.player2.throws } }

        SetShowResetModal show ->
            { model | showResetModal = show }

        SetScore player throw score ->
            let
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
            in
            case player of
                Msg.Player1 ->
                    { model | player1 = { throws = setThrows model.player1.throws, currentThrow = Nothing } }

                Msg.Player2 ->
                    { model | player2 = { throws = setThrows model.player2.throws, currentThrow = Nothing } }


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
        , resetModal model
        ]
