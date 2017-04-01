#r "../packages/NAudio/lib/net35/NAudio.dll"
#r "../packages/MathNet.Numerics/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp/lib/net40/MathNet.Numerics.FSharp.dll"

open NAudio.Midi

#load "Events.fsx"
#load "Charts.fsx"
open Midi.FSharp

let file = MidiFile("./MidiFiles/feel.mid")

let trackNames = [
    "Piano 1"
    "Bass"
    "Strings"
    "El.Guitar"
    "Piano 2"
    "Cymbal"
    "Drums"
]

file.Events
|> Seq.skip 1
|> getNotesPitchData
|> drawBarChart "Notes Pitch Stats" trackNames

file.Events
|> Seq.skip 1
|> getNotesLengthData
|> drawBarChart "Notes Length Stats" trackNames

