namespace Tweek.JPad.Parsing
open Tweek.JPad.AST;
open FSharpUtils.Newtonsoft;

module public ValueDistribution = 
    let private parseValueWithValueType (valueType:string) (value:string)= match valueType with
        | "boolean" -> JsonValue.Boolean (value |> bool.Parse);
        | "number" -> JsonValue.Number (value |> decimal);
        |  _ -> JsonValue.String value;

    let private parseWeightedArgs (args:JsonValue) (valueType:string) = match args with
        | JsonValue.Record record -> record |> Array.map (fun (k,v)-> (parseValueWithValueType valueType k, v.AsInteger()))
        | JsonValue.Array array -> array |> Array.map (fun item-> (item.["value"], item.["weight"].AsInteger()))

    let private parseValueDistribution (args:JsonValue) (valueType:string) distributionType = match distributionType with
        | "uniform" -> DistributionType.Uniform (args.AsArray())
        | "bernoulliTrial" -> DistributionType.Bernouli (args.AsFloat())
        | "weighted" -> DistributionType.Weighted (parseWeightedArgs args valueType)

    let parse valueType (jsonRule:JsonValue) = jsonRule.["type"] |> JsonExtensions.AsString |> (parseValueDistribution jsonRule.["args"] valueType)