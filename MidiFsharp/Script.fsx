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

let data = file.Events |> Seq.mapi (fun i ec -> (i, Seq.length ec))

Chart.Bar(data)
