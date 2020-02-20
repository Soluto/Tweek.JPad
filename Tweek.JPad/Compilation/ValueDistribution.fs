namespace Tweek.JPad.Compilation
open Tweek.JPad.AST
open System
open FSharpUtils.Newtonsoft
open Tweek.JPad
open System.Text

module public ValueDistribution = 
    let private uniformCalc (choices:JsonValue[]) (hash) = 
        let index  = (hash % (choices.Length |> uint64)) |> int
        choices.[index]

    let scanWithFirstItem fold seq = seq |> Seq.skip 1 |> Seq.scan fold (Seq.head seq)

    let private weightedCalc (weighted:(JsonValue*int)[]) (hash) = 
        let selectedItem = hash % (weighted |> Seq.sumBy snd |> uint64) |> int

        weighted
        |> scanWithFirstItem (fun (_,acc_weight) (next_val,weight) -> (next_val, acc_weight+weight))
        |> Seq.skipWhile (fun (_, range)-> selectedItem >= range )  
        |> Seq.map fst
        |> Seq.head

    let floatToWeighted = (*) 100.0 >> int

    let compile (distributionType:DistributionType) =
        let fn = match distributionType with
        | Uniform array ->  array |> uniformCalc;
        | Weighted weightedValues ->  weightedCalc weightedValues
        | Bernouli ratio ->  let percentage = ratio |>floatToWeighted
                             weightedCalc [|(JsonValue.Boolean(true), percentage);(JsonValue.Boolean(false), (100 - percentage))|];
        
        (fun (sha1Provider:Sha1Provider) (units : Object[])-> 
            let input = units |> Seq.map string |>  String.concat "."
            let hash = BitConverter.ToUInt64(((sha1Provider.Invoke (Encoding.UTF8.GetBytes input)).[0..15]), 0)
            fn(hash))