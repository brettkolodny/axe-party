module Model exposing (Model, Player, init, newPlayer)

import Fireworks exposing (Firework)
import Particle.System as System exposing (System)
import Random


type alias Player =
    { throws : List (Maybe Int)
    , currentThrow : Maybe Int
    }


type alias Model =
    { player1 : Player
    , player2 : Player
    , showResetModal : Bool
    , fireworks : System Firework
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
    , fireworks = System.init <| Random.initialSeed 0
    }
