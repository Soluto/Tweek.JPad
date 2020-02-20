namespace Tweek.JPad.Parsing
open Tweek.JPad.AST
open FSharpUtils.Newtonsoft
open Tweek.JPad

module public Tree = 
    let private parsePatternType (input:string) = if input = "*" then PatternType.Default else PatternType.Exact
    
    let rec private parsePatternBlock depth pattern (rulesData:(string * JsonValue)[]) valueType : PatternBlock =
            match pattern with
                | Exact -> 
                    rulesData |> 
                    Array.map (fun (partitionValue,rules) -> (partitionValue.ToLower(), parseRulesContainer (depth - 1) rules valueType) ) |>
                    Map.ofArray |>
                    Map
                | Default -> parseRulesContainer (depth - 1) (snd rulesData.[0]) valueType |> PatternBlock.Default      
                | Pattern -> raise (ParseError("complex patterns are not supported yet"))
    
    and private parseRulesContainer (depth) (rulesData:JsonValue) valueType : RulesContainer =
            match (rulesData) with
                | Array rules when (depth = 0) -> rules |> List.ofArray |> List.map (Rule.parse valueType) |> RulesList
                | value when (depth = 0) -> RulesList [(Empty, SingleVariant(value))]
                | Record r when (depth > 0) -> 
                    r |> Array.groupBy (fst >> parsePatternType) |>
                    Array.map (fun (patternType,data) -> parsePatternBlock depth patternType data valueType) |>
                    RulesByPartition
                | String s when (depth > 0) -> [|parsePatternBlock depth Default [|("*",String(s))|] valueType|]  |>
                                                         RulesByPartition
                
    and parse (json:JsonValue) : JPad =
        match json with
        | Record r->
            let partitions = (json.GetProperty "partitions") |> JsonExtensions.AsArray |> Array.map JsonExtensions.AsString;
            let rules = (json.GetProperty "rules");
            let valueType = defaultArg (json.TryGetProperty "valueType" |> Option.map JsonExtensions.AsString) "string"
            let defaultValue = json.TryGetProperty "defaultValue"

            { Partitions = partitions;
              Rules = parseRulesContainer partitions.Length rules valueType
              ValueType = valueType
              DefaultValue = defaultValue
              } 
        | Array jpad1rules -> jpad1rules |> List.ofArray |>
                                        List.map (Rule.parse "string") |>
                                        RulesList |>
                                        (fun rules->{Partitions = [||]; Rules = rules; ValueType = "string"; DefaultValue = None})