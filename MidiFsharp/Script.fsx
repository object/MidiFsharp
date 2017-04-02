#r "../packages/NAudio/lib/net35/NAudio.dll"
#r "../packages/MathNet.Numerics/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp/lib/net40/MathNet.Numerics.FSharp.dll"

open NAudio.Midi

#load "Events.fsx"
#load "Charts.fsx"
#load "Patches.fsx"

open Midi.FSharp

let file = MidiFile("./MidiFiles/Feel.mid")
//let file = MidiFile("./MidiFiles/HotelCalifornia.mid")

file
|> getEventsByChannel
|> Seq.map snd
|> getNotesPitchData
|> drawBarChart "Notes Pitch Stats" (getTrackNames file)

file
|> getEventsByChannel
|> Seq.map snd
|> getNotesLengthData
|> drawBarChart "Notes Length Stats" (getTrackNames file)

file
|> getEventsByChannel
|> Seq.map snd
|> getPitchChangesData
|> drawBarChart "Pitch Changes Stats" (getTrackNames file)

file
|> getEventsByChannel
|> Seq.map snd
|> getNotesPolyphonyData
|> drawBarChart "Notes Polyphony Stats" (getTrackNames file)
