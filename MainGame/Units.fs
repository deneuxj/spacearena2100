module SpaceArena2100.Units

open CleverRake.XnaUtils.Units

/// Time, 0.1ms
[<Measure>] type dms

/// Health units
[<Measure>] type Health

/// Score
[<Measure>] type Points

/// 0.1ms per 1s, int
let dmsPerS = 1000<dms/s>

/// 0.1ms per 1s, float32
let dmsPerS_f = 1000.0f<dms/s>
