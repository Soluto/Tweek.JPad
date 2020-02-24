namespace Tweek.JPad.Compilation
open Tweek.JPad.AST
open Tweek.JPad
open FSharpUtils.Newtonsoft

module public Tree = 
    let seqFirstMatch fn seq = seq |> Seq.map fn |> Seq.tryFind Option.isSome |> Option.bind id

    let compile (settings:ParserSettings) (jpad:JPad) :JPadEvaluateExt =
        let rec createRuleContainerEvaluator (rulesContainer:RulesContainer)  : JPadEvaluate =
            match rulesContainer with
                |RulesByPartition (partitionType, rules, defaultRules) ->  
                    let compiledRules = rules |> Map.map (fun k v->createRuleContainerEvaluator v) 
                    let compiledDefaultRules = createRuleContainerEvaluator defaultRules
                    
                    (fun context -> 
                        let partitionValue = context partitionType |> Option.map JsonExtensions.AsString
                        let rules = partitionValue |> Option.bind (fun v-> compiledRules |> Map.tryFind (v.ToLower())) |> Option.defaultValue compiledDefaultRules
                        rules context)

                |RulesList rules -> 
                    let rulesFns = rules |> List.map (Rule.compile settings);
                    (fun context-> rulesFns |> seqFirstMatch (fun rule -> rule context))
                             
        createRuleContainerEvaluator jpad.Rules |>
        (fun evaluator -> JPadEvaluateExt(fun context -> let result = evaluator context.Invoke
                                                         if result.IsSome then result else jpad.DefaultValue ))