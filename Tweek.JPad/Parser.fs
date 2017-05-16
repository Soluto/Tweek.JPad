namespace Tweek.JPad
open FSharp.Data
open System;
open Tweek.JPad.AST;
open FSharpUtils.Newtonsoft;

type public JPadEvaluateExt = delegate of ContextDelegate -> Option<JsonValue>

type public JPadParser(settings:ParserSettings) = 
    
    let seqFirstMatch fn seq = seq |> Seq.map fn |> Seq.tryFind Option.isSome |> Option.bind id
    //parsing
    let parsePatternType (input:string) = if input = "*" then PatternType.Default else PatternType.Exact
    
    let rec parsePatternBlock depth pattern (rulesData:(string * JsonValue)[]) valueType : PatternBlock =
            match pattern with
                | PatternType.Exact -> 
                    rulesData |> 
                    Array.map (fun (partitionValue,rules) -> (partitionValue.ToLower(), parseRulesContainer (depth - 1) rules valueType) ) |>
                    Map.ofArray |>
                    PatternBlock.Map
                | PatternType.Default -> parseRulesContainer (depth - 1) (snd rulesData.[0]) valueType |> PatternBlock.Default      
                | PatternType.Pattern -> raise (ParseError("complex patterns are not supported yet"))
    
    and parseRulesContainer (depth) (rulesData:JsonValue) valueType : RulesContainer =
            match (rulesData) with
                | JsonValue.Array rules when (depth = 0) -> rules |> List.ofArray |> List.map (Rule.parse valueType) |> RulesContainer.RulesList
                | value when (depth = 0) -> RulesContainer.RulesList [(MatcherExpression.Empty, SingleVariant(value))]
                | JsonValue.Record r when (depth > 0) -> 
                    r |> Array.groupBy (fst >> parsePatternType) |>
                    Array.map (fun (patternType,data) -> parsePatternBlock depth patternType data valueType) |>
                    RulesContainer.RulesByPartition
                | JsonValue.String s when (depth > 0) -> [|parsePatternBlock depth PatternType.Default [|("*",JsonValue.String(s))|] valueType|]  |>
                                                         RulesContainer.RulesByPartition
                
    and buildAST (json:JsonValue) : JPad =
        match json with
        | JsonValue.Record r->
            let partitions = (json.GetProperty "partitions") |> JsonExtensions.AsArray |> Array.map JsonExtensions.AsString;
            let rules = (json.GetProperty "rules");
            let valueType = defaultArg (json.TryGetProperty "valueType" |> Option.map JsonExtensions.AsString) "string"
            let defaultValue = json.TryGetProperty "defaultValue"

            { Partitions = partitions;
              Rules = parseRulesContainer partitions.Length rules valueType
              ValueType = valueType
              DefaultValue = defaultValue
              } 
        | JsonValue.Array jpad1rules -> jpad1rules |> List.ofArray |>
                                        List.map (Rule.parse "string") |>
                                        RulesContainer.RulesList |>
                                        (fun rules->{Partitions = [||]; Rules = rules; ValueType = "string"; DefaultValue = None})
    //--

    let extractPartitionValue partitionType (context:Context) = 
        defaultArg ((context partitionType) |> Option.map JsonExtensions.AsString) "*"

    //evaluating
    let rec createRuleContainerEvaluator (partitions:List<string>) (rulesContainer:RulesContainer)  : JPadEvaluate =
        match rulesContainer, partitions with
                |RulesByPartition rules, (partitionType :: otherPartitions) ->  
                    let compiledBlocks = rules |> Array.map (evaluatePatternBlock otherPartitions)
                    let partitionExtractor = extractPartitionValue partitionType
                    
                    (fun context -> 
                        let partitionValue = context |> partitionExtractor
                        compiledBlocks |> seqFirstMatch (fun block -> block partitionValue context))

                |RulesList rules, [] -> 
                    let rulesFns = rules |> List.map (Rule.buildEvaluator settings);
                    (fun context-> rulesFns |> seqFirstMatch (fun rule -> rule context))
                                                
    
     and evaluatePatternBlock partitions block : (string)->JPadEvaluate  =
        match block with
            |Map map ->  let compiledMap = map |> Map.map (fun _ fn -> createRuleContainerEvaluator partitions fn)
                         (fun partitionValue -> defaultArg (compiledMap |> Map.tryFind (partitionValue.ToLower())) (fun _->None) )
            |PatternBlock.Default container -> let fn = createRuleContainerEvaluator partitions container
                                               (fun partitionValue -> fn)
    //--
    
    //api
    let buildEvaluator (jpad:JPad) :JPadEvaluateExt =
        createRuleContainerEvaluator (jpad.Partitions |> List.ofArray) jpad.Rules |>
        (fun evaluator -> JPadEvaluateExt(fun context -> let result = evaluator context.Invoke
                                                         if result.IsSome then result else jpad.DefaultValue ))
    
    member public this.Parse : (string -> JPadEvaluateExt) = 
        JsonValue.Parse >>
        buildAST >>
        buildEvaluator
    //--
    