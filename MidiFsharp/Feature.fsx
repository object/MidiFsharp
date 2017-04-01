#I "../packages/NAudio/lib/net35"
#r "NAudio.dll"

namespace Midi.Features

[<AutoOpen>]
module Utils =

    open NAudio.Midi

    type FeatureValue = float

    type IFeature = 
        abstract member Calculate: MidiFile -> float 
        abstract member Similarity: FeatureValue * FeatureValue -> float

    type FeatureVector = FeatureValue * FeatureValue


    let magnitude (f: FeatureVector) =
        let fv1, fv2 = f
        float fv1 * fv2 

    let dotProduct (f1: FeatureVector, f2: FeatureVector) =
        let f1V1, f1V2 = f1
        let f2V1, f2V2 = f2
        float(f1V1 * f1V2 + f2V1 * f2V2)
        
    let cosineSimilarity (f1: FeatureVector, f2: FeatureVector) =
        float (dotProduct(f1, f2) / (magnitude(f1) * magnitude(f2)))

    type TestFeature() =
        interface IFeature with
            member this.Similarity(f1, f2) =
                f1 * f2
            member this.Calculate(f) =
                0.2
    


