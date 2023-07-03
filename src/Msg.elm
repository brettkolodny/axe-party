module Msg exposing (Msg(..), Player(..))

import Fireworks exposing (Firework)
import Particle.System as System


type Player
    = Player1
    | Player2


type Msg
    = SetScore Player (Maybe Int) (Maybe Int)
    | ResetGame
    | SetCurrentThrow Player (Maybe Int)
    | SetShowResetModal Bool
    | Detonate
    | ParticleMsg (System.Msg Firework)
