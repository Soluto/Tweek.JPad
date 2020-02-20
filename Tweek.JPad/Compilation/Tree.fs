namespace Tweek.JPad.Compilation
open Tweek.JPad.AST
open Tweek.JPad
open FSharpUtils.Newtonsoft

module public Tree = 
    let seqFirstMatch fn seq = seq |> Seq.map fn |> Seq.tryFind Option.isSome |> Option.bind id

    let extractPartitionValue partitionType (context:Context) = 
        defaultArg ((context partitionType) |> Option.map JsonExtensions.AsString) "*"

    let compile (settings:ParserSettings) (jpad:JPad) :JPadEvaluateExt =
        let rec createRuleContainerEvaluator (partitions:List<string>) (rulesContainer:RulesContainer)  : JPadEvaluate =
            match rulesContainer, partitions with
                |RulesByPartition rules, (partitionType :: otherPartitions) ->  
                    let compiledBlocks = rules |> Array.map (evaluatePatternBlock otherPartitions)
                    let partitionExtractor = extractPartitionValue partitionType
                    
                    (fun context -> 
                        let partitionValue = context |> partitionExtractor
                        compiledBlocks |> seqFirstMatch (fun block -> block partitionValue context))

                |RulesList rules, [] -> 
                    let rulesFns = rules |> List.map (Rule.compile settings);
                    (fun context-> rulesFns |> seqFirstMatch (fun rule -> rule context))
                                                
    
        and evaluatePatternBlock partitions block : (string)->JPadEvaluate  =
            match block with
                |Map map ->  let compiledMap = map |> Map.map (fun _ fn -> createRuleContainerEvaluator partitions fn)
                             (fun partitionValue -> defaultArg (compiledMap |> Map.tryFind (partitionValue.ToLower())) (fun _->None) )
                |PatternBlock.Default container -> let fn = createRuleContainerEvaluator partitions container
                                                   (fun _ -> fn)

        createRuleContainerEvaluator (jpad.Partitions |> List.ofArray) jpad.Rules |>
        (fun evaluator -> JPadEvaluateExt(fun context -> let result = evaluator context.Invoke
                                                         if result.IsSome then result else jpad.DefaultValue ))