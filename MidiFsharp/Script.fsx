#r "../packages/NAudio/lib/net35/NAudio.dll"

open NAudio.Midi

let file = MidiFile("./MidiFiles/feel.mid")

printfn "%d" file.Tracks
printfn "%A" (file.Events |> Seq.length)
file.Events
|> Seq.iter (printf "%d\n" << Seq.length)

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

let getTrackEvents trackNo numberOfEvents =
    file.Events
    |> Seq.skip (trackNo-1)
    |> Seq.head
    |> Seq.truncate numberOfEvents

let printTrackEvents trackNo numberOfEvents =
    getTrackEvents trackNo numberOfEvents
    |> (fun ec -> 
        printfn "%d" (ec |> Seq.length)
        ec |> Seq.iter printMidiEvent)

let getTrackEventsByType<'a when 'a :> MidiEvent> trackNo =
    file.Events
    |> Seq.skip (trackNo-1)
    |> Seq.head
    |> Seq.choose (fun x-> 
        x |> function 
            | :? 'a as e -> Some e 
            | _ -> None)

getTrackEventsByType<NoteOnEvent> 2 |> Seq.averageBy (fun x -> (float)x.AbsoluteTime)

printTrackEvents 8 10
