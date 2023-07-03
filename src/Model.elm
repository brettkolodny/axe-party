module Model exposing (Model, Player, init, newPlayer)


type alias Player =
    { throws : List (Maybe Int)
    , currentThrow : Maybe Int
    }


type alias Model =
    { player1 : Player
    , player2 : Player
    , showResetModal : Bool
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


init : Model
init =
    { player1 = newPlayer
    , player2 = newPlayer
    , showResetModal = False
    }
