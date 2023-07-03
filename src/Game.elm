module Game exposing (game)

import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra
import Model exposing (Model)
import Msg exposing (Msg(..))
import VitePluginHelper


game : Model -> Msg.Player -> Html Msg
game model player =
    let
        ( playerDisplay, throws, currentThrow ) =
            case player of
                Msg.Player1 ->
                    ( "Player 1", model.player1.throws, model.player1.currentThrow )

                Msg.Player2 ->
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


scoreButton : Msg.Player -> Int -> Maybe Int -> Bool -> Html Msg
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


throwsDisplay : Msg.Player -> List (Maybe Int) -> Maybe Int -> Html Msg
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
