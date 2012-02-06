module SpaceArena2100.Units

open CleverRake.XnaUtils.Units

/// Time, 0.1ms
[<Measure>] type dms

/// Health units
[<Measure>] type Health

/// Score
[<Measure>] type Points

/// 0.1ms per 1s, int
let dmsPerS = 10000<dms/s>

/// 0.1ms per 1s, float32
let dmsPerS_f = 10000.0f<dms/s>

/// Create an int<dms> from a float32<s>
let dmsFromS (s : float32<s>) = intFromFloat32 (s * dmsPerS_f)

/// Units of numerical ids assigned by the XNA framework.
type [<Measure>] LivePlayer
