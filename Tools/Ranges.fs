namespace AdventOfCode

module Ranges =
    type Range64 = {
        Start: int64
        End: int64
    }
    with
        member this.IsInRange x = x >= this.Start && x <= this.End
        member this.PositionInRange x = 
            match this.IsInRange x with
            | true -> Some (x - this.Start)
            | false -> None
        member this.Middle () = this.Start + (this.End - this.Start) / 2L
        member this.Intersection other =
            if other.Start > this.End then
                None
            else if other.End < this.Start then
                None
            else
                if other.IsInRange(this.Start) then
                    if other.IsInRange(this.End) then
                        Some this
                    else
                        Some { Start = this.Start; End = other.End }
                else if this.IsInRange(other.Start) then
                    if this.IsInRange(other.End) then
                        Some other
                    else
                        Some { Start = other.Start; End = this.End }
                else
                    failwithf "Invalid range intersection"
