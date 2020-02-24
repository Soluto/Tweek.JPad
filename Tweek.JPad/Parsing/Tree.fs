namespace Tweek.JPad.Parsing
open Tweek.JPad.AST
open FSharpUtils.Newtonsoft
open Tweek.JPad

module public Tree = 
    let rec private parseRulesContainer valueType (partitions:List<string>) (rulesData:JsonValue)  : RulesContainer =
            match (rulesData, partitions) with
                | Array rules, [] -> rules |> List.ofArray |> List.map (Rule.parse valueType) |> RulesList
                | value, [] -> RulesList [(Empty, SingleVariant(value))]
                | Record r, partition::other -> 
                    let defaultValue = r |> Array.tryFind (fst >> (=) "*") |> Option.map (fun (_,v)-> parseRulesContainer valueType other v)
                                         |> Option.defaultValue (RulesList [])
                    let map = r |> Seq.filter (fst >> (<>) "*") |> Seq.map (fun (k,v) -> ((k.ToLower()), (parseRulesContainer valueType other v))) |> Map.ofSeq
                    RulesByPartition (partition, map, defaultValue)
                | value, partition::other -> 
                        RulesByPartition (partition, Map.empty, (parseRulesContainer valueType other value ))
                
    let parse (json:JsonValue) : JPad =
        match json with
        | Record r->
            let partitions = (json.GetProperty "partitions") |> JsonExtensions.AsArray |> Seq.map JsonExtensions.AsString |> List.ofSeq;
            let rules = (json.GetProperty "rules");
            let valueType = defaultArg (json.TryGetProperty "valueType" |> Option.map JsonExtensions.AsString) "string"
            let defaultValue = json.TryGetProperty "defaultValue"

            { 
              Rules = parseRulesContainer valueType partitions rules
              ValueType = valueType
              DefaultValue = defaultValue
              } 
        | Array jpad1rules -> jpad1rules |> List.ofArray |>
                                        List.map (Rule.parse "string") |>
                                        RulesList |>
                                        (fun rules->{Rules = rules; ValueType = "string"; DefaultValue = None})