#I "../packages/NAudio/lib/net35"
#I "../packages/MathNet.Numerics/lib/net40"
#I "../packages/MathNet.Numerics.FSharp/lib/net40"

#r "NAudio.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"

#load "Patches.fsx"

namespace Midi.FSharp

[<AutoOpen>]
module Events =

    open NAudio.Midi
    open MathNet.Numerics.Statistics

    let getEventsByChannel (file : MidiFile) =
        file.Events
        |> Seq.concat
        |> Seq.groupBy (fun e -> e.Channel)
 
    let getEventsByType<'a when 'a :> MidiEvent> (events : seq<MidiEvent>) =
        events
        |> Seq.choose (fun x-> 
            x |> function 
                | :? 'a as e -> Some e
                | _ -> None)

    let getNotes (events : seq<MidiEvent>) =
        events
        |> getEventsByType<NoteOnEvent>
        |> Seq.filter (fun e -> not << isNull <| e.OffEvent)

    let getPitchChanges (events : seq<MidiEvent>) =
        events
        |> getEventsByType<PitchWheelChangeEvent>

    let getPatches (events : seq<MidiEvent>) =
        events
        |> getEventsByType<PatchChangeEvent>

    let isDrumChannel (events : seq<MidiEvent>) =
        (not << Seq.isEmpty) events && events |> Seq.head |> fun x -> x.Channel = 10 

    let getChannelPatch (events : seq<MidiEvent>) =
        events
        |> getPatches
        |> fun x -> if Seq.isEmpty x then 
                        None
                    else
                        Seq.head x |> fun y -> Some y.Patch

    let getTrackNames file =
        file 
        |> getEventsByChannel
        |> Seq.map (fun (ch, es) -> 
            match getChannelPatch es with
            | Some patch -> sprintf "%s" (getPatchName patch)
            | None -> if isDrumChannel es then "Drums" else "Unknown")

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

    let getPitchChangesStatistics (events : seq<MidiEvent>) =
        events
        |> getPitchChanges
        |> Seq.map (fun e -> float e.Pitch)
        |> fun ec -> DescriptiveStatistics(ec)

    let private getIntervalDistributions intervals =
        let getSplitPoints intervals =
            let rec splitIntervals intervals =
                match intervals with
                | [] -> []
                | (x,y)::tail -> x::y::splitIntervals tail
            intervals
            |> splitIntervals
            |> List.sort
            |> Set.ofList
            |> Set.toList

        let getSplitPointsWeights intervals points =
            let within (x, y) v = v >= x && v < y
            points
            |> List.map (fun pt -> 
                (pt, intervals |> List.fold (fun acc (x,y) -> 
                    if pt |> within (x,y) then acc+1 else acc) 0))
        
        let intervals = intervals |> Seq.toList
        let points = 
            intervals
            |> Seq.toList
            |> getSplitPoints
        let weightedPoints = getSplitPointsWeights intervals points
        points
        |> List.pairwise
        |> List.map (fun (x,y) -> ((x,y), weightedPoints |> List.find (fun (p,w) -> p = x) |> fun (p,w) -> w))
        |> List.filter (fun (_, w) -> w <> 0)
        |> List.toSeq

    let getNotesPolyphony (events : seq<MidiEvent>) =
        events
        |> getNotes
        |> Seq.sortBy (fun e -> e.AbsoluteTime)
        |> Seq.map (fun e -> (e.AbsoluteTime, e.AbsoluteTime+(int64 e.NoteLength)))
        |> getIntervalDistributions

    let getNotesPolyphonyStatistics (events : seq<MidiEvent>) =
        events
        |> getNotesPolyphony
        |> Seq.map (fun ((x,y),w) -> float w)
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
