#I "../packages/NAudio/lib/net35"
#I "../packages/MathNet.Numerics/lib/net40"
#I "../packages/MathNet.Numerics.FSharp/lib/net40"

#load "../packages/FsLab/FsLab.fsx"

#r "NAudio.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"

#load "Events.fsx"

namespace Midi.FSharp

[<AutoOpen>]
module Charts =

    open NAudio.Midi
    open XPlot.GoogleCharts
    open MathNet.Numerics.Statistics

    let notePitchFeatures = [|
        "Range"
        "Mean"
        "Min"
        "Max"
        "Variance"
        "StdDev"
    |]

    let getNotesPitchData events = 
        events
        |> Seq.map (fun ec -> 
            let stats = getNotesPitchStatistics ec
            [
                (notePitchFeatures.[0], stats.Maximum - stats.Minimum)
                (notePitchFeatures.[1], stats.Mean)
                (notePitchFeatures.[2], stats.Minimum)
                (notePitchFeatures.[3], stats.Maximum)
                (notePitchFeatures.[4], stats.Variance)
                (notePitchFeatures.[5], stats.StandardDeviation)
            ])

    let notesLengthFeatures = [|
        "Mean"
        "Min"
        "Max"
        "Variance"
        "StdDev"
    |]
    let getNotesLengthData events = 
        events
        |> Seq.map (fun ec -> 
            let stats = getNotesLengthStatistics ec
            [
                (notesLengthFeatures.[0], stats.Mean)
                (notesLengthFeatures.[1], stats.Minimum)
                (notesLengthFeatures.[2], stats.Maximum)
                (notesLengthFeatures.[3], stats.Variance)
                (notesLengthFeatures.[4], stats.StandardDeviation)
            ])

    let drawBarChart title trackNames data =
        data
        |> Chart.Combo
        |> Chart.WithOptions (
            Options(
                title = title, 
                series = Array.init (Seq.length trackNames) (fun x -> Series("bars"))))
        |> Chart.WithLabels trackNames
        |> Chart.WithLegend true
        |> Chart.WithSize (1200, 500)
