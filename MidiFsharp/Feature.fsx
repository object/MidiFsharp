#I "../packages/NAudio/lib/net35"
#r "NAudio.dll"

namespace Midi.Features

[<AutoOpen>]
module Features =

    open NAudio.Midi

    type FeatureValue = float

    type RawFeatureValue = 
        | FloatValue of float
        | IntValue of int

    let getFeatureValue x =
        match x with 
        | FloatValue x -> x
        | IntValue x -> float x

    type Feature(calculateFun) =
            member this.Calculate(f) = calculateFun(f)

    type FeatureVector = FeatureValue * FeatureValue

    type FeaturePair = FeatureValue 

    let extractFeatures (set1, set2) =
        Seq.zip set1 set2
        |> Seq.map(fun featurePair ->
            match featurePair with
            | ((_, feature1), (_, feature2)) -> (feature1, feature2))

    let magnitude (f: FeatureVector) =
        let fv1, fv2 = f
        sqrt(pown fv1 2 + pown fv2 2)
    // Tests
    magnitude((4.0, 3.0)) // 5.0

    let dotProduct (f1: FeatureVector, f2: FeatureVector) =
        let f1V1, f1V2 = f1
        let f2V1, f2V2 = f2
        f1V1 * f2V1 + f1V2 * f2V2

    // Tests
    dotProduct((3.0, 2.0), (3.0, 2.0)) // 13.0

    let cosineSimilarity (f1: FeatureVector, f2: FeatureVector) =
        dotProduct(f1, f2) / (magnitude(f1) * magnitude(f2))

    // Tests
    cosineSimilarity((3.0, 2.0), (3.0, 2.0)) // 1.0

    let createSimilarityMatrix (featureVectors) = 
        featureVectors 
            |> Seq.map (fun fv -> featureVectors |> Seq.map(fun fv2 -> cosineSimilarity(fv, fv2)))
     
    let robbie = MidiFile("./MidiFiles/feel.mid")
    let elton = MidiFile("./MidiFiles/DontGoBreakingMyHeart.mid")

    let feature1 = Feature(fun a -> 2.3)
    let testVector1 = (feature1.Calculate(robbie), feature1.Calculate(robbie))

