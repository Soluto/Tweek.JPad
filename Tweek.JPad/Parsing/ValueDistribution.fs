namespace Tweek.JPad.Parsing
open Tweek.JPad.AST;
open FSharpUtils.Newtonsoft;

module public ValueDistribution = 
    let private parseValueWithValueType (valueType:string) (value:string)= 
        match valueType with
        | "boolean" -> Boolean (value |> bool.Parse);
        | "number" -> Number (value |> decimal);
        |  _ -> String value;

    let private parseWeightedArgs (args:JsonValue) (valueType:string) = 
        match args with
        | Record record -> record |> Array.map (fun (k,v)-> (parseValueWithValueType valueType k, v.AsInteger()))
        | Array array -> array |> Array.map (fun item-> (item.["value"], item.["weight"].AsInteger()))

    let private parseValueDistribution (args:JsonValue) (valueType:string) distributionType = 
        match distributionType with
        | "uniform" -> Uniform (args.AsArray())
        | "bernoulliTrial" -> Bernouli (args.AsFloat())
        | "weighted" -> Weighted (parseWeightedArgs args valueType)

    let parse valueType (jsonRule:JsonValue) = jsonRule.["type"] |> JsonExtensions.AsString |> (parseValueDistribution jsonRule.["args"] valueType)