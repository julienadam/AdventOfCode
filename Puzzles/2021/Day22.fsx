

#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open Tools

type Range = int*int
type Cuboid = Range * Range * Range
type RebootInstruction = Cuboid * bool

let mapLine line = 
    let regex = new Regex("(?<state>on|off) x=(?<xmin>-?\d+)\.\.(?<xmax>-?\d+),y=(?<ymin>-?\d+)\.\.(?<ymax>-?\d+),z=(?<zmin>-?\d+)\.\.(?<zmax>-?\d+)")
    let m = regex.Match(line)
    let active = match m.Groups.["state"].Value with | "on" -> true | "off" -> false | _ -> failwithf "invalid pixel state"
    let xRange = m |> mInt "xmin", m |> mInt "xmax"
    let yRange = m |> mInt "ymin", m |> mInt "ymax"
    let zRange = m |> mInt "zmin", m |> mInt "zmax"
    let (instruction:RebootInstruction) = (xRange, yRange, zRange), active
    instruction

mapLine "off x=-54112..-39298,y=-85059..-49293,z=-27449..7877"