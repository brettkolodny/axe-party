module ResetModal exposing (..)

import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg(..))


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
