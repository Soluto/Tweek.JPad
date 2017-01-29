﻿namespace Tweek.JPad
open FSharp.Data;
open System.Security;
open Tweek.JPad.Grammer;
open System.Text
open System;
open FSharpUtils.Newtonsoft;

module Matcher = 

    let nullProtect x = match x with |null -> None |_-> Some x;

    let (|KeyProperty|Operator|) (input:string) = if input.[0] = '$' then Operator else KeyProperty

    let private reducePredicate op seq = if Seq.isEmpty(seq) then true else seq |> Seq.reduce(op)               

    let private reduceOrElse reduceFun alt seq = if not (Seq.isEmpty(seq)) then seq|> Seq.reduce reduceFun else alt

    let private parseOp op : Op = match op with
        |"$not" -> Op.Not
        |"$or" -> Op.ConjuctionOp(ConjuctionOp.Or)
        |"$and" -> Op.ConjuctionOp(ConjuctionOp.And)
        |"$ge" -> Op.CompareOp(CompareOp.GreaterEqual)
        |"$eq" -> Op.CompareOp(CompareOp.Equal)
        |"$gt" -> Op.CompareOp(CompareOp.GreaterThan)
        |"$le" -> Op.CompareOp(CompareOp.LessEqual)
        |"$lt" -> Op.CompareOp(CompareOp.LessThan)
        |"$ne" -> Op.CompareOp(CompareOp.NotEqual)
        |"$in" -> Op.ArrayOp(ArrayOp.In)
        | s -> raise (ParseError("expected operator, found:"+s))

    let private evaluateComparisonOp = function 
                    | CompareOp.Equal -> (fun b a -> a = b)
                    | CompareOp.GreaterThan -> (fun b a -> a > b)
                    | CompareOp.LessThan -> (fun b a -> a < b)
                    | CompareOp.GreaterEqual -> (fun b a -> a >= b)
                    | CompareOp.LessEqual -> (fun b a -> a <= b)
                    | CompareOp.NotEqual -> (fun b a -> a <> b)

    type private comparer = (JsonValue)->(JsonValue)->int

    let private createComparer (fn:(string-> IComparable)) l = 
        let target = (
            try
                fn(l)
            with
                | ex -> ParseError ("failure in parser value:" + l.ToString()) |> raise
            )
        target.CompareTo << fn;
    
    let private evaluateComparison (comparer) (op: CompareOp) (leftValue:ComparisonValue) =
        match (leftValue) with
            | JsonValue.String l -> (comparer l) |> (fun comparison rightValue -> 
                match rightValue with 
                | None -> false 
                |Some json -> match json with 
                    |JsonValue.String v -> evaluateComparisonOp op (comparison v) 0
                    | _ -> false)
            | _ -> (fun (rightValueOption:Option<ComparisonValue>) -> match (leftValue, op, rightValueOption) with
                | JsonValue.Null, CompareOp.Equal, None -> true
                | JsonValue.Null, CompareOp.NotEqual, None -> false
                | _, CompareOp.Equal, None -> false
                | _, CompareOp.NotEqual, None -> true
                | _, _, Some rightValue -> match (leftValue, op,  rightValue) with
                            | JsonValue.Null, CompareOp.Equal, JsonValue.Null -> true
                            | JsonValue.Null, CompareOp.NotEqual, JsonValue.Null -> false
                            | _, CompareOp.Equal, JsonValue.Null -> false
                            | _, CompareOp.NotEqual, JsonValue.Null -> true
                            | JsonValue.Null, CompareOp.Equal, _ -> false
                            | JsonValue.Null, CompareOp.NotEqual, _ -> true
                            | JsonValue.Number x, _ , JsonValue.Number y -> evaluateComparisonOp op x y
                            | JsonValue.Number x, _ , JsonValue.String y -> decimal y |> evaluateComparisonOp op x 
                            | JsonValue.Boolean x, _ ,JsonValue.Boolean y -> evaluateComparisonOp op x y
                            | JsonValue.Boolean x, _ ,JsonValue.String y -> bool.Parse y |> evaluateComparisonOp op x
                            | JsonValue.Float x, _ ,JsonValue.Float y -> evaluateComparisonOp op x y
                            | JsonValue.Float x, _ ,JsonValue.String y -> float y |> evaluateComparisonOp op x
                            | _ , _ , _ -> Exception("non matching types") |> raise
                        )

    let private evaluateArrayTest (comparer) (op: ArrayOp) (jsonValue:ComparisonValue) : (Option<JsonValue>->bool) =
        match jsonValue with
            | JsonValue.Array arr -> match op with
                | ArrayOp.In ->  
                let compareItem = evaluateComparison comparer CompareOp.Equal
                (fun contextValue -> arr |> Array.exists (fun item-> compareItem item contextValue ))
            | _ -> (fun _->false)

    let rec private parsePropertySchema (conjuctionOp : ConjuctionOp) (schema:JsonValue)  : MatcherExpression = 
        match schema with 
        | JsonValue.Record record -> 
            let converterType = record |> Seq.tryFind (fst >> (=) "$compare")
            let filter = (match converterType with |None -> id |Some x -> Seq.filter ((<>) x) )
            let props = record |> 
                filter |>
                Seq.map (fun (key,innerSchema)-> match key with 
                    |KeyProperty-> MatcherExpression.Property(key, innerSchema |> parsePropertySchema ConjuctionOp.And)
                    |Op-> match parseOp(key) with
                        | Op.CompareOp compareOp -> MatcherExpression.Compare (compareOp, innerSchema)
                        | Op.ArrayOp arrayOp -> MatcherExpression.ArrayTest (arrayOp, innerSchema)
                        | Op.ConjuctionOp binaryOp-> match binaryOp with
                            | ConjuctionOp.And -> innerSchema |> parsePropertySchema ConjuctionOp.And
                            | ConjuctionOp.Or  -> innerSchema |> parsePropertySchema ConjuctionOp.Or
                        | Op.Not  -> MatcherExpression.Not(innerSchema |> parsePropertySchema ConjuctionOp.And)
                ) |> reduceOrElse (fun acc exp-> MatcherExpression.Binary(conjuctionOp, acc, exp)) MatcherExpression.Empty
            match converterType with 
                |Some (_, convertType) -> MatcherExpression.SwitchComparer( convertType.AsString(), props)
                |None -> props
        | x -> MatcherExpression.Compare(CompareOp.Equal, x)

    let getPropName prefix prop = if prefix = "" then prop else (prefix + "." + prop)

    let private compile_internal (comparers:System.Collections.Generic.IDictionary<string,ComparerDelegate>) exp  = 
        let getComparer s = if comparers.ContainsKey(s) then Some comparers.[s] else None;
        let rec CompileExpression (prefix:string) comparer (exp: MatcherExpression)  : (Context) -> bool =
            match exp with
                | Property (prop, innerexp) -> CompileExpression (getPropName prefix prop) comparer innerexp 
                | MatcherExpression.Not (innerexp) ->  CompileExpression prefix comparer innerexp >> not
                | MatcherExpression.Binary (op, l, r) -> 
                    let lExp = CompileExpression prefix comparer l;
                    let rExp = CompileExpression prefix comparer r;
                    match op with
                    |ConjuctionOp.And -> fun c-> (lExp c) && (rExp c)
                    |ConjuctionOp.Or -> fun c->  (lExp c) || (rExp c)
                | ArrayTest (op, op_value) ->  
                    (|>) prefix >> evaluateArrayTest comparer op op_value  
                | Compare (op, op_value) ->  
                    (|>) prefix >> evaluateComparison comparer op op_value
                | SwitchComparer (newComparer, innerexp) -> match getComparer(newComparer) with
                    | Some inst_comparer -> innerexp|> CompileExpression prefix (createComparer(inst_comparer.Invoke))
                    | None -> ParseError ("missing comparer - " + newComparer) |> raise
                | Empty -> (fun context->true)

        let defaultComparer (l:string) (r:string) = l.ToLower().CompareTo (r.ToLower())
        CompileExpression "" defaultComparer exp

    let parse (schema:JsonValue) = parsePropertySchema ConjuctionOp.And schema

    let createEvaluator (settings: ParserSettings) (matcher: MatcherExpression) =
        ( matcher |> (compile_internal settings.Comparers))

module ValueDistribution = 
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

    let compile (schema:JsonValue) =
        let fn = match schema.GetProperty("type").AsString() with
        | "uniform" ->  schema.GetProperty("args").AsArray() |> uniformCalc;
        | "weighted" ->  let weightedValues = match schema.GetProperty("args") with  
                             | JsonValue.Array r -> r |> Array.map (fun(item) -> (item.["value"], item.["weight"].AsInteger()))
                             | JsonValue.Record r -> r |> Array.map (fun (k,v)-> (JsonValue.String(k), v.AsInteger()))
                         weightedCalc weightedValues
        | "bernoulliTrial" -> let percentage = schema.GetProperty("args").AsFloat() |>floatToWeighted
                              weightedCalc [|(JsonValue.Boolean(true), percentage);(JsonValue.Boolean(false), (100 - percentage))|];
        | s -> raise (Exception("expected operator, found:"+s));
        
        (fun (sha1Provider:Sha1Provider) (units : Object[])-> 
            let input = units |> Seq.map string |>  String.concat "."
            let hash = BitConverter.ToUInt64(((sha1Provider (Encoding.UTF8.GetBytes input)).[0..15]), 0)
            fn(hash))
    
module Rule = 
    let parse (jsonRule:JsonValue) = 
        let matcher = jsonRule.["Matcher"] |> Matcher.parse;
        let value = match jsonRule.["Type"].AsString() with
                            | "SingleVariant" -> RuleValue.SingleVariant(jsonRule.["Value"])
                            | "MultiVariant" ->  
                                RuleValue.MultiVariant({
                                                        HashFunction = jsonRule.["ValueDistribution"] |> ValueDistribution.compile;
                                                        OwnerType = jsonRule.TryGetProperty("OwnerType") |> Option.map JsonExtensions.AsString
                                                        Salt = jsonRule.["Id"].AsString()
                                                        })
                            | _ -> raise (ParseError("not supported value distrubtion"))

        (matcher, value)

    let buildEvaluator (settings:ParserSettings) (rule : (MatcherExpression * RuleValue)) : JPadEvaluate =
        let matcher = Matcher.createEvaluator settings (fst rule);
        let validateMatcher context = if (matcher context) then Some(context) else None;  
        match (snd rule) with
            |SingleVariant value -> validateMatcher >> Option.map (fun _ -> value);
            |MultiVariant valueDistribution -> 
                let hash = valueDistribution.HashFunction settings.Sha1Provider
                validateMatcher >> Option.bind (fun context->
                let opOwner = valueDistribution.OwnerType |> Option.map (fun owner -> owner + ".@@id") |> Option.bind context;
                opOwner |> Option.map (fun s-> s.AsString()) |> Option.map (fun owner -> hash [|owner :> Object;valueDistribution.Salt :> Object|])
            )
