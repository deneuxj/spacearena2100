module SpaceArena2100.Units

/// Position, meters
[<Measure>] type m

/// Time, seconds
[<Measure>] type s

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

/// An array whose index has a unit of measure
type MarkedArray<[<Measure>] 'K, 'T> = MarkedArray of 'T[]
with
    member this.Item
        with get (i : int<'K>) =
            let (MarkedArray arr) = this
            arr.[int i]
        and set (i : int<'K>) (v : 'T) =
            let (MarkedArray arr) = this
            arr.[int i] <- v
