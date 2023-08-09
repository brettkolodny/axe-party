port module Main exposing (main)

import Browser
import Fireworks exposing (Color(..), Firework, fireworkAt, fireworkView)
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Json.Decode as D
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
    | SetShowOptionsModal Bool
    | Detonate
    | ParticleMsg (Particle.System.Msg Firework)
    | SetNumThrows Int
    | SwapSides



-- Model


type alias Player =
    { throws : List (Maybe Int)
    , currentThrow : Maybe Int
    }


type alias GameState =
    { player1 : Player
    , player2 : Player
    , numThrows : Int
    , swapSides : Bool
    }


type alias Model =
    { gameState : GameState
    , showResetModal : Bool
    , showOptionsModal : Bool
    , fireworks : Particle.System.System Firework
    }


newPlayer : Int -> Player
newPlayer numThrows =
    let
        throws =
            List.repeat numThrows Nothing
    in
    { throws = throws
    , currentThrow = Nothing
    }


newModel : Model
newModel =
    { gameState =
        { player1 = newPlayer 5
        , player2 = newPlayer 5
        , numThrows = 5
        , swapSides = False
        }
    , showResetModal = False
    , showOptionsModal = False
    , fireworks = Particle.System.init <| Random.initialSeed 0
    }



-- Main


main : Program (Maybe String) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \model -> Particle.System.sub [] ParticleMsg model.fireworks }



-- Ports


port saveState : GameState -> Cmd msg



-- Init


init : Maybe String -> ( Model, Cmd Msg )
init maybeGameStateString =
    case maybeGameStateString of
        Just gameStateString ->
            let
                decodePlayer =
                    D.map2
                        Player
                        (D.field "throws" <| D.list <| D.maybe D.int)
                        (D.field "currentThrow" <| D.maybe <| D.int)

                decodeGameState =
                    D.map4
                        GameState
                        (D.field "player1" decodePlayer)
                        (D.field "player2" decodePlayer)
                        (D.field "numThrows" D.int)
                        (D.field "swapSides" D.bool)
            in
            case D.decodeString decodeGameState gameStateString of
                Ok gameState ->
                    ( { newModel | gameState = gameState }, Cmd.none )

                Err _ ->
                    ( newModel, saveState newModel.gameState )

        Nothing ->
            ( newModel, saveState newModel.gameState )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetGame ->
            let
                gameState =
                    newModel.gameState

                numThrows =
                    model.gameState.numThrows

                newGameState =
                    { gameState | numThrows = numThrows, player1 = newPlayer numThrows, player2 = newPlayer numThrows }
            in
            ( { newModel | gameState = newGameState }, saveState newModel.gameState )

        SetCurrentThrow role throw ->
            let
                gameState =
                    model.gameState

                updatePlayerCurrentThrow player =
                    { player | currentThrow = throw }

                newGameState =
                    case role of
                        Player1 ->
                            { gameState | player1 = updatePlayerCurrentThrow gameState.player1 }

                        Player2 ->
                            { gameState | player2 = updatePlayerCurrentThrow gameState.player2 }
            in
            ( { model | gameState = newGameState }, saveState newGameState )

        SwapSides ->
            let
                gameState =
                    model.gameState

                newGameState =
                    { gameState | swapSides = not gameState.swapSides }
            in
            ( { model | gameState = newGameState }, saveState newGameState )

        SetNumThrows numThrows ->
            let
                setPlayerThrows player =
                    if List.length player.throws > numThrows then
                        List.take numThrows player.throws

                    else
                        player.throws ++ List.repeat (numThrows - List.length player.throws) Nothing

                player1 =
                    model.gameState.player1

                newPlayer1 =
                    { player1 | throws = setPlayerThrows player1 }

                player2 =
                    model.gameState.player2

                newPlayer2 =
                    { player2 | throws = setPlayerThrows player2 }

                gameState =
                    model.gameState

                newGameState =
                    { gameState
                        | numThrows = numThrows
                        , player1 = newPlayer1
                        , player2 = newPlayer2
                    }
            in
            ( { model
                | gameState = newGameState
              }
            , saveState newGameState
            )

        SetShowResetModal show ->
            ( { model | showResetModal = show }, Cmd.none )

        SetShowOptionsModal show ->
            ( { model | showOptionsModal = show }, Cmd.none )

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

        SetScore role throw score ->
            let
                gameState =
                    model.gameState

                playerThrows =
                    case role of
                        Player1 ->
                            gameState.player1.throws

                        Player2 ->
                            gameState.player2.throws

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
                        |> (\total -> total == model.gameState.numThrows * 5)

                updatePlayerThrows player =
                    { player | throws = setThrows player.throws, currentThrow = Nothing }

                newGameState =
                    case role of
                        Player1 ->
                            { gameState | player1 = updatePlayerThrows gameState.player1 }

                        Player2 ->
                            { gameState | player2 = updatePlayerThrows gameState.player2 }

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
            ( { model | gameState = newGameState }, Cmd.batch [ saveState newGameState, cmd ] )



-- View


view : Model -> Html Msg
view model =
    let
        gameBoard =
            if model.gameState.swapSides then
                [ game model.gameState Player2, game model.gameState Player1 ]

            else
                [ game model.gameState Player1, game model.gameState Player2 ]
    in
    div
        [ class "flex flex-col justify-center items-center min-w-screen min-h-screen bg-thunder-900 gap-12"
        , style "background-image" ("url('" ++ VitePluginHelper.asset "/src/assets/chains.svg" ++ "')")
        ]
        [ button [ onClick <| SetShowOptionsModal True, class "fixed right-4 top-4 px-3 py-2 bg-thunder-400 text-white" ] [ text "Options" ]
        , div [ class "flex items-center justify-evenly w-full max-w-[1000px]" ]
            gameBoard
        , div [ class "flex items-center gap-8" ]
            [ button
                [ onClick SwapSides, class "w-[256px] h-12 bg-thunder-600 font-bold text-white text-[24px]" ]
                [ text "Swap Sides" ]
            , button
                [ onClick <| SetShowResetModal True, class "w-[256px] h-12 bg-red-600 font-bold text-white text-[24px]" ]
                [ text "Reset Match" ]
            ]
        , Particle.System.view fireworkView [ style "width" "100vw", style "height" "100vh", style "position" "fixed", style "pointer-events" "none", style "top" "0", style "left" "0" ] model.fireworks
        , resetModal model
        , optionsModal model
        ]


game : GameState -> Role -> Html Msg
game gameState player =
    let
        ( playerDisplay, throws, currentThrow ) =
            case player of
                Player1 ->
                    ( "Player 1", gameState.player1.throws, gameState.player1.currentThrow )

                Player2 ->
                    ( "Player 2", gameState.player2.throws, gameState.player2.currentThrow )

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
                "w-full h-12 bg-hatchets-green-600 font-bold text-white text-[24px]"

            else
                "w-full h-12 bg-hatchets-green-600/50 font-bold text-thunder-50 text-[24px] cursor-not-allowed"
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
    div [ class "flex items-center justify-between flex-wrap w-[320px] gap-4" ]
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


optionsModal : Model -> Html Msg
optionsModal model =
    if model.showOptionsModal then
        div [ class "fixed flex items-center justify-center w-screen h-screen inset-0 bg-black/50" ]
            [ div [ class "flex flex-col justify-center items-center gap-6 bg-thunder-50 p-8" ]
                [ h2 [ class "text-2xl font-bold" ] [ text "Options" ]
                , div [ class "flex items-center justify-between gap-12 w-full" ]
                    [ button [ onClick <| SetNumThrows <| model.gameState.numThrows - 1, class "bg-hatchets-green text-white py-2 w-20" ] [ text "Remove" ]
                    , p [ class "text-2xl" ] [ text <| String.fromInt model.gameState.numThrows ++ " throws" ]
                    , button [ onClick <| SetNumThrows <| model.gameState.numThrows + 1, class "bg-hatchets-green text-white py-2 w-20" ] [ text "Add" ]
                    ]
                , button [ onClick <| SetShowOptionsModal False, class "px-3 py-2 bg-thunder-400 text-white" ] [ text "Close" ]
                ]
            ]

    else
        div [ class "hidden" ] []



-- Utility


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
