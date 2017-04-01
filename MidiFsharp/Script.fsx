#r "../packages/NAudio/lib/net35/NAudio.dll"
#r "../packages/XPlot.GoogleCharts/lib/net45/Xplot.GoogleCharts.dll"

#load "../packages/FsLab/FsLab.fsx"

open NAudio.Midi
open XPlot.GoogleCharts

#load "./Utils.fsx"
open Midi.Utils

let file = MidiFile("./MidiFiles/feel.mid")

printfn "%d" file.Tracks
printfn "%A" (file.Events |> Seq.length)
file.Events
|> Seq.iter (printf "%d\n" << Seq.length)

file |> getTrackEventsByType<NoteOnEvent> 8 |> Seq.truncate 2 |> Seq.averageBy (fun x -> (float)x.AbsoluteTime)

file |> printTrackEvents 8
let notesRange = file.Events|> Seq.skip 1 |> Seq.mapi (fun i ec -> sprintf "%d" (i+2), ec |> getNotesRangeWidth)
let notesAvgPitch = file.Events|> Seq.skip 1 |> Seq.mapi (fun i ec -> sprintf "%d" (i+2), ec |> getNotesAvgPitch)
let notesAvgLength = file.Events|> Seq.skip 1 |> Seq.mapi (fun i ec -> sprintf "%d" (i+2), ec |> getNotesAvgLength)

file |> getTrackEvents 3 |> getNotesRange

let trackNames = [
    "Piano 1"
    "Bass"
    "Strings"
    "El.Guitar"
    "Piano 2"
    "Cymbal"
    "Drums"
]

let features = [|
    "Range"
    "Min P"
    "Max P"
    "Avg P"
//    "Avg L"
|]

let series = [ "bars";"bars";"bars";"bars";"bars";"bars";"bars" ]
let inputs = 
    file.Events
    |> Seq.skip 1
    |> Seq.map (fun ec -> 
        [
            (features.[0], getNotesRangeWidth ec |> float)
            (features.[1], getNotesMinPitch ec |> float)
            (features.[2], getNotesMaxPitch ec |> float)
            (features.[3], getNotesAvgPitch ec)
//            (features.[4], getNotesAvgLength ec)
        ])


inputs
|> Chart.Combo
|> Chart.WithOptions 
     (Options(title = "Notes Stats", series = 
        [| for typ in series -> Series(typ) |]))
|> Chart.WithLabels trackNames
|> Chart.WithLegend true
|> Chart.WithSize (600, 250)

Chart.Bar(notesRange)
Chart.Bar(notesAvgPitch)
Chart.Bar(notesAvgLength)
