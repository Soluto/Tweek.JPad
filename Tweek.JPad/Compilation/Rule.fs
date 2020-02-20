namespace Tweek.JPad.Compilation
open Tweek.JPad.AST;
open System;
open FSharpUtils.Newtonsoft;
open Tweek.JPad

module public Rule = 
    let compile (settings:ParserSettings) (rule : (MatcherExpression * RuleValue)) : JPadEvaluate =
        let matcher = Matcher.compile settings (fst rule);
        let validateMatcher context = if (matcher context) then Some(context) else None;  
        match (snd rule) with
            |SingleVariant value -> validateMatcher >> Option.map (fun _ -> value);
            |MultiVariant valueDistribution -> 
                let hashFunction = ValueDistribution.compile valueDistribution.DistributionType;
                let hash = hashFunction settings.Sha1Provider
                validateMatcher >> Option.bind (fun context->
                let opOwner = valueDistribution.OwnerType |> Option.map (fun owner -> owner + ".@@id") |> Option.bind context;
                opOwner |> Option.map (fun s-> s.AsString()) |> Option.map (fun owner -> hash [|owner :> Object;valueDistribution.Salt :> Object|])
            )