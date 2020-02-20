namespace Tweek.JPad.Parsing
open Tweek.JPad.AST;
open FSharpUtils.Newtonsoft;
open Tweek.JPad

module public Rule = 

    let parse valueType (jsonRule:JsonValue) = 
        let matcher = jsonRule.["Matcher"] |> Matcher.parse;
        let value = match jsonRule.["Type"].AsString() with
                            | "SingleVariant" -> SingleVariant(jsonRule.["Value"])
                            | "MultiVariant" -> MultiVariant({
                                DistributionType = ValueDistribution.parse valueType jsonRule.["ValueDistribution"] 
                                OwnerType = jsonRule.TryGetProperty("OwnerType") |> Option.map JsonExtensions.AsString
                                Salt = (match jsonRule.TryGetProperty("Salt") with | None -> jsonRule.["Id"] | Some v -> v) |> JsonExtensions.AsString
                                })
                            | _ -> raise (ParseError("not supported value distrubtion"))
        (matcher, value)