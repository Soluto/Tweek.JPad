namespace Tweek.JPad
open FSharp.Data;
open System.Security;
open Tweek.JPad.AST;
open System.Text;
open System.Text.RegularExpressions;
open System;
open FSharpUtils.Newtonsoft;

module Matcher = 

    let inline toMap kvps = kvps |> Seq.map (|KeyValue|) |> Map.ofSeq
    let nullProtect x = match x with |null -> None |_-> Some x;

    let (|KeyProperty|Operator|) (input:string) = if input.[0] = '$' then Operator else KeyProperty

    let private reducePredicate op seq = if Seq.isEmpty(seq) then true else seq |> Seq.reduce(op)               

    let private reduceOrElse reduceFun alt seq = if not (Seq.isEmpty(seq)) then seq|> Seq.reduce reduceFun else alt

    let private parseOp op : Op = match op with
        |"$not" -> Op.Not
        |"$or" -> Op.ConjuctionOp(ConjuctionOp.Or)
        |"$and" -> Op.ConjuctionOp(ConjuctionOp.And)
        |"$ge" -> Op.BinaryOp(BinaryOp.CompareOp(CompareOp.GreaterEqual))
        |"$eq" -> Op.BinaryOp(BinaryOp.CompareOp(CompareOp.Equal))
        |"$gt" -> Op.BinaryOp(BinaryOp.CompareOp(CompareOp.GreaterThan))
        |"$le" -> Op.BinaryOp(BinaryOp.CompareOp(CompareOp.LessEqual))
        |"$lt" -> Op.BinaryOp(BinaryOp.CompareOp(CompareOp.LessThan))
        |"$ne" -> Op.BinaryOp(BinaryOp.CompareOp(CompareOp.NotEqual))
        |"$in" -> Op.BinaryOp(BinaryOp.In)
        |"$contains" -> Op.BinaryOp(BinaryOp.StringOp(StringOp.Contains))
        |"$startsWith" -> Op.BinaryOp(BinaryOp.StringOp(StringOp.StartsWith))
        |"$endsWith" -> Op.BinaryOp(BinaryOp.StringOp(StringOp.EndsWith))
        |"$withinTime" -> Op.BinaryOp(BinaryOp.TimeOp(TimeOp.WithinTime))
        | s -> raise (ParseError("expected operator, found:"+s))

    let inline evaluateComparisonOp op a b = match op with
                    | CompareOp.Equal -> a = b
                    | CompareOp.GreaterThan -> a < b
                    | CompareOp.LessThan -> a > b
                    | CompareOp.GreaterEqual ->  a <= b
                    | CompareOp.LessEqual -> a >= b
                    | CompareOp.NotEqual -> a <> b

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

    let private (|Suffix|_|) (p:string) (s:string) =
        if s.EndsWith(p) then
            Some(s.Substring(0, s.Length - p.Length))
        else
            None

    let private parseTimeUnit (value:string) = 
        match value with
        | Suffix "s" unitValue -> unitValue |> float |> TimeSpan.FromSeconds
        | Suffix "m" unitValue -> unitValue |> float |> TimeSpan.FromMinutes
        | Suffix "h" unitValue -> unitValue |> float |> TimeSpan.FromHours
        | Suffix "d" unitValue -> unitValue |> float |> TimeSpan.FromDays
        | _ -> Exception("Invalid time unit") |> raise

    let private parseTime (valueOption:Option<JsonValue>) = 
        match valueOption with
        | None -> None
        | Some value -> 
            match value with
            | JsonValue.String stringValue -> stringValue |> DateTime.Parse |> Some
            | _ -> Exception("Invalid time format") |> raise

    let private evaluateTimeComparison (prefix) (op: TimeOp) (leftValue:ComparisonValue)  =
        match (op) with 
            | TimeOp.WithinTime -> 
                let timespan = parseTimeUnit (leftValue.AsString())
                (fun (context:Context) -> 
                    let systemTime = parseTime(context("system.time_utc"))
                    let fieldValueOption = parseTime(context(prefix))
                    match (systemTime, fieldValueOption) with
                    | _, None -> false
                    | None, _ -> Exception("Missing system time details") |> raise
                    | Some now, Some fieldValue -> (now - fieldValue).Duration() < timespan)

    let falseOnNone fn opt = match opt with |Some v -> if v = null then false else fn v 
                                            |None -> false

    let private evaluateStringComparison (op: StringOp) (leftValue:string) =
        let casedValue = leftValue.ToLower()
        match (op) with
            | StringOp.Contains -> (fun (s:string) ->s.ToLower().Contains casedValue ) |> falseOnNone
            | StringOp.StartsWith -> (fun (s:string) ->s.ToLower().StartsWith casedValue ) |> falseOnNone
            | StringOp.EndsWith -> (fun (s:string) ->s.ToLower().EndsWith casedValue ) |> falseOnNone

    let private evaluateInArray (comparer) (jsonValue:ComparisonValue) : (Option<JsonValue>->bool) =
        match jsonValue with
            | JsonValue.Array arr -> 
                let compareItem = evaluateComparison comparer CompareOp.Equal
                (fun contextValue -> arr |> Array.exists (fun item-> compareItem item contextValue ))
            | _ -> (fun _->false)

    let rec private parsePropertySchema (conjuctionOp : ConjuctionOp)  (comparisonType:ComparisonType) (schema:JsonValue)  : MatcherExpression = 
        match schema with 
        | JsonValue.Record record -> 
            let converterType = record |> Seq.tryFind (fst >> (=) "$compare")
            let filter = (match converterType with |None -> id |Some x -> Seq.filter ((<>) x) )
            let newComparisonType = match converterType with 
                                        |Some (_, convertType) -> ComparisonType.Custom (convertType |> JsonExtensions.AsString)
                                        |None -> comparisonType
            record |> 
                filter |>
                Seq.map (fun (key,innerSchema)-> match key with 
                    |KeyProperty-> MatcherExpression.Property(key, innerSchema |> parsePropertySchema ConjuctionOp.And newComparisonType)
                    |Op-> match parseOp(key) with
                        | Op.BinaryOp op -> MatcherExpression.Binary (op, newComparisonType, innerSchema)
                        | Op.ConjuctionOp op-> match op with
                            | ConjuctionOp.And -> innerSchema |> parsePropertySchema ConjuctionOp.And newComparisonType
                            | ConjuctionOp.Or  -> innerSchema |> parsePropertySchema ConjuctionOp.Or newComparisonType
                        | Op.Not  -> MatcherExpression.Not(innerSchema |> parsePropertySchema ConjuctionOp.And newComparisonType)
                ) |> reduceOrElse (fun acc exp-> MatcherExpression.Conjuction(conjuctionOp, acc, exp)) MatcherExpression.Empty
        | x -> MatcherExpression.Binary(BinaryOp.CompareOp(CompareOp.Equal), comparisonType,  x)

    let getPropName prefix prop = if prefix = "" then prop else (prefix + "." + prop)



    let private compile_internal (comparers:Map<string,ComparerDelegate>) exp  = 
        let defaultComparer (l:string) (r:string) = l.ToLower().CompareTo (r.ToLower())
        let getComparer comparisonType = match comparisonType with
                                         | Auto -> defaultComparer
                                         | Custom s -> if comparers.ContainsKey(s) then createComparer(comparers.[s].Invoke) else ParseError("missing comparer - " + s) |> raise

        let rec CompileExpression (prefix:string) (exp: MatcherExpression)  : (Context) -> bool =
            match exp with
                | Property (prop, innerexp) -> CompileExpression (getPropName prefix prop) innerexp 
                | MatcherExpression.Not (innerexp) ->  CompileExpression prefix innerexp >> not
                | MatcherExpression.Conjuction (op, l, r) -> 
                    let lExp = CompileExpression prefix l;
                    let rExp = CompileExpression prefix r;
                    match op with
                    |ConjuctionOp.And -> fun c-> (lExp c) && (rExp c)
                    |ConjuctionOp.Or -> fun c->  (lExp c) || (rExp c)
                | Binary (op, comparisonType, op_value) ->
                    let comaprer = (getComparer comparisonType)
                    match (op, op_value) with
                    | BinaryOp.In, _ -> (|>) prefix >> evaluateInArray comaprer op_value  
                    | BinaryOp.CompareOp compare_op, _ -> (|>) prefix >> evaluateComparison comaprer compare_op op_value
                    | BinaryOp.TimeOp time_op, _ -> evaluateTimeComparison prefix time_op op_value
                    | BinaryOp.StringOp string_op, JsonValue.String string_value -> (|>) prefix >> Option.map JsonExtensions.AsString >> evaluateStringComparison string_op string_value
                    | _ -> raise (ParseError "invalid binary matcher")
                | Empty -> (fun context->true)

        
        CompileExpression "" exp

    let parse (schema:JsonValue) = parsePropertySchema ConjuctionOp.And ComparisonType.Auto schema 

    let createEvaluator (settings: ParserSettings) (matcher: MatcherExpression) =
        let comparersMaps = (settings.Comparers |> toMap).Add("date", new ComparerDelegate(fun x -> DateTime.Parse(x) :> IComparable))
        (matcher |> (compile_internal comparersMaps))

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

    let parseValueWithValueType (valueType:string) (value:string)= match valueType with
        | "boolean" -> JsonValue.Boolean (value |> bool.Parse);
        | "number" -> JsonValue.Number (value |> decimal);
        |  _ -> JsonValue.String value;

    let parseValueDistribution (args:JsonValue) (valueType:string)  distributionType = match distributionType with
        | "uniform" -> DistributionType.Uniform (args.AsArray())
        | "bernoulliTrial" -> DistributionType.Bernouli (args.AsFloat())
        | "weighted" -> DistributionType.Weighted (args.Properties() |> Array.map (fun (k,v)-> (parseValueWithValueType valueType k, v.AsInteger())))

    let parse valueType (jsonRule:JsonValue) = jsonRule.["type"] |> JsonExtensions.AsString |> (parseValueDistribution jsonRule.["args"] valueType)

    let compile (distributionType:DistributionType) =
        let fn = match distributionType with
        | Uniform array ->  array |> uniformCalc;
        | Weighted weightedValues ->  weightedCalc weightedValues
        | Bernouli ratio ->  let percentage = ratio |>floatToWeighted
                             weightedCalc [|(JsonValue.Boolean(true), percentage);(JsonValue.Boolean(false), (100 - percentage))|];
        
        (fun (sha1Provider:Sha1Provider) (units : Object[])-> 
            let input = units |> Seq.map string |>  String.concat "."
            let hash = BitConverter.ToUInt64(((sha1Provider.Invoke (Encoding.UTF8.GetBytes input)).[0..15]), 0)
            fn(hash))
    
module Rule = 
    let parse valueType (jsonRule:JsonValue) = 
        let matcher = jsonRule.["Matcher"] |> Matcher.parse;
        let value = match jsonRule.["Type"].AsString() with
                            | "SingleVariant" -> RuleValue.SingleVariant(jsonRule.["Value"])
                            | "MultiVariant" -> RuleValue.MultiVariant({
                                DistributionType = ValueDistribution.parse valueType jsonRule.["ValueDistribution"] 
                                OwnerType = jsonRule.TryGetProperty("OwnerType") |> Option.map JsonExtensions.AsString
                                Salt = (match jsonRule.TryGetProperty("Salt") with | None -> jsonRule.["Id"] | Some v -> v) |> JsonExtensions.AsString
                                })
                            | _ -> raise (ParseError("not supported value distrubtion"))
        (matcher, value)

    let buildEvaluator (settings:ParserSettings) (rule : (MatcherExpression * RuleValue)) : JPadEvaluate =
        let matcher = Matcher.createEvaluator settings (fst rule);
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

