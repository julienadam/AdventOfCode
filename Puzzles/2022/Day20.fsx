#time
#load "../../Tools.fsx"

open System
open System.Text.RegularExpressions
open System.IO
open System.Threading
open System.Collections.Generic
open Checked
open Tools


let getInput p =
    File.ReadAllLines(getInputPath2022 p)
    |> Seq.map Int32.Parse

getInput "Day20_sample1.txt"

