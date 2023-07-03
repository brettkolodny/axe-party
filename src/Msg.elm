module Msg exposing (Msg(..), Player(..))


type Player
    = Player1
    | Player2


type Msg
    = SetScore Player (Maybe Int) (Maybe Int)
    | ResetGame
    | SetCurrentThrow Player (Maybe Int)
    | SetShowResetModal Bool
