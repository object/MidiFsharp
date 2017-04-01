#I "../packages/NAudio/lib/net35"
#I "../packages/MathNet.Numerics/lib/net40"
#I "../packages/MathNet.Numerics.FSharp/lib/net40"

#r "NAudio.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"

namespace Midi.FSharp

[<AutoOpen>]
module Events =

    open NAudio.Midi
    open MathNet.Numerics.Statistics

    let getTrackEvents trackNo (file : MidiFile) =
        file.Events
        |> Seq.skip (trackNo-1)
        |> Seq.head

    let getEventsByType<'a when 'a :> MidiEvent> (events : seq<MidiEvent>) =
        events
        |> Seq.choose (fun x-> 
            x |> function 
                | :? 'a as e -> Some e
                | _ -> None)

    let getTrackEventsByType<'a when 'a :> MidiEvent> trackNo (file : MidiFile) =
        file.Events
        |> Seq.skip (trackNo-1)
        |> Seq.head
        |> getEventsByType<'a>

    let getNotes (events : seq<MidiEvent>) =
        events
        |> getEventsByType<NoteOnEvent>
        |> Seq.filter (fun e -> not << isNull <| e.OffEvent)

    let getNotesPitchStatistics (events : seq<MidiEvent>) =
        events
        |> getNotes
        |> Seq.map (fun e -> float e.NoteNumber)
        |> fun ec -> DescriptiveStatistics(ec)

    let getNotesLengthStatistics (events : seq<MidiEvent>) =
        events
        |> getNotes
        |> Seq.map (fun e -> float e.NoteLength)
        |> fun ec -> DescriptiveStatistics(ec)

    let printMidiEvent (e : MidiEvent) =
        printfn "Event: %A (%A) on channel %A at abs/delta time %d/%d" (e.GetType()) e.CommandCode e.Channel e.AbsoluteTime e.DeltaTime
        match e with
        | :? TextEvent as e ->
            printfn "\tText: %s" e.Text
        | :? TimeSignatureEvent as e ->
            printfn "\tDenominator/numerator: %d/%d" e.Denominator e.Numerator 
            printfn "\tNumber of 32 notes in quarter note: %d" e.No32ndNotesInQuarterNote
            printfn "\tTicks in metronome click: %d" e.TicksInMetronomeClick
            printfn "\tTime signature: %s" e.TimeSignature
        | :? KeySignatureEvent as e ->
            printfn "\tMajor/minor: %d" e.MajorMinor
            printfn "\tSharps/flats: %d" e.SharpsFlats
        | :? TempoEvent as e ->
            printfn "\tMicroseconds per quarter note: %d" e.MicrosecondsPerQuarterNote
            printfn "\tTempo: %f" e.Tempo
        | :? PatchChangeEvent as e ->
            printfn "\tPatch: %d" e.Patch
        | :? ControlChangeEvent as e ->
            printfn "\tController: %A" e.Controller
            printfn "\tController value: %d" e.ControllerValue
        | :? PitchWheelChangeEvent as e ->
            printfn "\tPitch: %A" e.Pitch
        | :? NoteOnEvent as e ->
            printfn "\tNote name: %s" e.NoteName
            printfn "\tNote number: %d" e.NoteNumber
            printfn "\tVelocity: %d" e.Velocity
            printfn "\tOff event: %A" e.OffEvent
            if not (isNull e.OffEvent) then
                printfn "\tNote length: %d" e.NoteLength
                printfn "\t\tOff note name: %s" e.OffEvent.NoteName
                printfn "\t\tOff note number: %d" e.OffEvent.NoteNumber
                printfn "\t\tOff velocity: %d" e.OffEvent.Velocity
        | :? MetaEvent as e ->
            printfn "\tMeta event type: %A" e.MetaEventType
        | _ ->
            printfn "\t UNKNOWN EVENT"
            printfn "\tAbsolute/delta time: %d/%d" e.AbsoluteTime e.DeltaTime

    let printTrackEvents trackNo (file : MidiFile) =
        file
        |> getTrackEvents trackNo
        |> (fun ec -> 
            printfn "%d" (ec |> Seq.length)
            ec |> Seq.iter printMidiEvent)
