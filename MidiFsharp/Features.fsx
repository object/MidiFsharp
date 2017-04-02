#I "../packages/MathNet.Numerics/lib/net40"
#I "../packages/MathNet.Numerics.FSharp/lib/net40"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"

namespace Midi.FSharp

[<AutoOpen>]
module Features =
    
    open MathNet.Numerics.LinearAlgebra

    let zipSequences (sequences) =
        sequences
        |> Seq.collect Seq.indexed 
        |> Seq.groupBy fst 
        |> Seq.map (snd >> Seq.map snd)
    
    let extractFeatureVectors (features) =
        features
        |> Seq.map (fun dataSet ->
            CreateVector.DenseOfEnumerable(dataSet |> Seq.map (snd)))
    
    let combineTrackData d1 d2 =
        d1 
        |> Seq.mapi (fun i trackData ->
            (Seq.append trackData (Seq.item i d2)))

    let sumVectors vecs = 
        vecs
        |> CreateMatrix.DenseOfRowVectors
        |> Matrix.sumCols

    let centroidVector vectors =
        let cardinality = float(Seq.length vectors)
        vectors
        |> sumVectors
        |> Vector.map(fun x -> x / cardinality)

    let magnitude (f) =
        sqrt(Seq.fold(fun (x) next -> x + pown next 2) 0.0 f)
    // Tests
    magnitude([|4.0; 3.0|]) // 5.0

    let dotProduct (a, b) =
        Seq.zip a b
        |> Seq.fold(fun (x) next ->
            let (v1, v2) = next
            x + v1 * v2) 0.0

    // Tests
    dotProduct([|3.0; 2.0|], [|3.0; 2.0|]) // 13.0

    let cosineSimilarity (f1, f2) =
        dotProduct(f1, f2) / (magnitude(f1) * magnitude(f2))

    // Tests
    cosineSimilarity([|3.0; 2.0|], [|3.0; 2.0|]) // 1.0


    let createSimilarityMatrix (featureVectors) = 
        featureVectors 
        |> Seq.map (fun fv -> 
            featureVectors
            |> Seq.map(fun otherFv -> 
                cosineSimilarity(fv, otherFv)))
     
