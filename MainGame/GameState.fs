module GameState

open Microsoft.Xna.Framework

/// Data that does not change often during a round.
type Description =
    { numPlayers : int;
      localPlayersIds : int list;
      localAiPlayerIds : int list;
      remotePlayerIds : int list;
      deadPlayerIds : int list;
      asteroidPos : Vector3[];
      asteroidRadius : float32[];
      asteroidRotX : float32[];
      asteroidRotY : float32[];
    }

/// Data that changes every frame.
type State =
    { heading : Vector3[];
      right : Vector3[];
      /// Position interpolated from our data.
      posClient : Vector3[];
      /// Position interpolated from the latest data received from the host of the player.
      posHost : Vector3[];
      speed : Vector3[];
      health : float32[];
      score : float32[];
      bulletPos : Vector3[];
      bulletTimeLeft : int[];
      bulletSpeed : Vector3[];
      bulletRadius : float32[];
      bulletOwner : int[];
      time : int;
    }