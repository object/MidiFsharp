#r "../packages/NAudio/lib/net35/NAudio.dll"
#r "../packages/MathNet.Numerics/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp/lib/net40/MathNet.Numerics.FSharp.dll"

open NAudio.Midi

#load "Events.fsx"
#load "Charts.fsx"
#load "Patches.fsx"
#load "Features.fsx"

open Midi.FSharp

let file = MidiFile("./MidiFiles/Feel.mid")
let hotel = MidiFile("./MidiFiles/HotelCalifornia.mid")

let pitchData = 
    file
    |> getEventsByChannel
    |> Seq.map snd
    |> getNotesPitchData

let noteLengthData =
    file
    |> getEventsByChannel
    |> Seq.map snd
    |> getNotesLengthData

noteLengthData |> drawBarChart "Note Length Stats" (getTrackNames file)

// file
// |> getEventsByChannel
// |> Seq.map snd
// |> getPitchChangesData
// |> drawBarChart "Pitch Changes Stats" (getTrackNames file)

let feelData = combineTrackData noteLengthData pitchData
let featureVectors = feelData |> extractFeatureVectors |> Seq.toArray
featureVectors



// let hotelVectors = 
let centroidVector = featureVectors |> centroidVector
centroidVector
let similarityMatrix = featureVectors |> createSimilarityMatrix
similarityMatrix

getSimilarityChartData similarityMatrix ((getTrackNames file) |> Seq.toList)
|> drawBarChart "Similarities" (getTrackNames file)


