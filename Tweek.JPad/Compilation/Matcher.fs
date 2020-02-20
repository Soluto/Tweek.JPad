namespace Tweek.JPad.Compilation
open Tweek.JPad.AST;
open System;
open FSharpUtils.Newtonsoft;
open Tweek.JPad

module public Matcher = 
    let private falseOnNone fn opt = match opt with |Some v -> if v = null then false else fn v 
                                                    |None -> false

    let inline private toMap kvps = kvps |> Seq.map (|KeyValue|) |> Map.ofSeq
    let private createComparer (fn:(string-> IComparable)) l = 
        let target = (
            try
                fn(l)
            with
                | ex -> ParseError ("failure in parser value:" + l.ToString()) |> raise
            )
        target.CompareTo << fn;
    
    let private getPropName prefix prop = if prefix = "" then prop else (prefix + "." + prop)

    let private parseTime (valueOption:Option<JsonValue>) = 
        match valueOption with
        | None -> None
        | Some value -> 
            match value with
            | JsonValue.String stringValue -> stringValue |> DateTime.Parse |> Some
            | _ -> Exception("Invalid time format") |> raise

    let inline private evaluateComparisonOp op a b = match op with
                    | Equal -> a = b
                    | GreaterThan -> a < b
                    | LessThan -> a > b
                    | GreaterEqual ->  a <= b
                    | LessEqual -> a >= b
                    | NotEqual -> a <> b

    let private evaluateComparison (comparer) (op: CompareOp) (leftValue:ComparisonValue) =
        match (leftValue) with
            | JsonValue.String l -> (comparer l) |> (fun comparison rightValue -> 
                match rightValue with 
                | None -> false 
                |Some json -> match json with 
                    |JsonValue.String v -> evaluateComparisonOp op (comparison v) 0
                    | _ -> false)
            | _ -> (fun (rightValueOption:Option<ComparisonValue>) -> match (leftValue, op, rightValueOption) with
                | Null, Equal, None -> true
                | Null, NotEqual, None -> false
                | _, Equal, None -> false
                | _, NotEqual, None -> true
                | _, _, Some rightValue -> match (leftValue, op,  rightValue) with
                            | Null, Equal, Null -> true
                            | Null, NotEqual, Null -> false
                            | _, Equal, Null -> false
                            | _, NotEqual, Null -> true
                            | Null, Equal, _ -> false
                            | Null, NotEqual, _ -> true
                            | Number x, _ , Number y -> evaluateComparisonOp op x y
                            | Number x, _ , JsonValue.String y -> decimal y |> evaluateComparisonOp op x 
                            | JsonValue.Boolean x, _ ,JsonValue.Boolean y -> evaluateComparisonOp op x y
                            | JsonValue.Boolean x, _ ,JsonValue.String y -> bool.Parse y |> evaluateComparisonOp op x
                            | Float x, _ ,JsonValue.Float y -> evaluateComparisonOp op x y
                            | Float x, _ ,JsonValue.String y -> float y |> evaluateComparisonOp op x
                            | _ , _ , _ -> Exception("non matching types") |> raise
                        )

    let private evaluateStringComparison (op: StringOp) (leftValue:string) =
        let casedValue = leftValue.ToLower()
        match (op) with
            | Contains -> (fun (s:string) ->s.ToLower().Contains casedValue ) |> falseOnNone
            | StartsWith -> (fun (s:string) ->s.ToLower().StartsWith casedValue ) |> falseOnNone
            | EndsWith -> (fun (s:string) ->s.ToLower().EndsWith casedValue ) |> falseOnNone

    let private evaluateTimeComparison (prefix) (op: TimeOp) (timespan:TimeSpan)  =
        match (op) with 
            | TimeOp.WithinTime -> 
                (fun (context:Context) -> 
                    let systemTime = parseTime(context("system.time_utc"))
                    let fieldValueOption = parseTime(context(prefix))
                    match (systemTime, fieldValueOption) with
                    | _, None -> false
                    | None, _ -> Exception("Missing system time details") |> raise
                    | Some now, Some fieldValue -> (now - fieldValue).Duration() < timespan)

    let private evaluateInArray (comparer) (arr:ComparisonValue[]) : (Option<JsonValue>->bool) =
        let compareItem = evaluateComparison comparer Equal
        (fun contextValue -> arr |> Array.exists (fun item-> compareItem item contextValue ))

    let private compile_internal (comparers:Map<string,ComparerDelegate>) exp  = 
            let defaultComparer (l:string) (r:string) = l.ToLower().CompareTo (r.ToLower())
            let getComparer comparisonType = match comparisonType with
                                             | Auto -> defaultComparer
                                             | Custom s -> if comparers.ContainsKey(s) then createComparer(comparers.[s].Invoke) else ParseError("missing comparer - " + s) |> raise

            let rec CompileExpression (prefix:string) (exp: MatcherExpression) : (Context) -> bool =
                match exp with
                    | Property (prop, innerexp) -> CompileExpression (getPropName prefix prop) innerexp 
                    | Op o -> match o with
                        | Op.Not (innerexp) -> CompileExpression prefix innerexp >> not
                        | ConjuctionOp (op, l, r)->
                            let lExp = CompileExpression prefix l;
                            let rExp = CompileExpression prefix r;
                            match op with
                            |And -> fun c-> (lExp c) && (rExp c)
                            |Or -> fun c->  (lExp c) || (rExp c)
                        | CompareOp (op,value,newComparisonType) -> (|>) prefix >> evaluateComparison (getComparer newComparisonType) op value
                        | In (value,newComparisonType) -> (|>) prefix >> evaluateInArray (getComparer newComparisonType) value 
                        | TimeOp (op, value) -> evaluateTimeComparison prefix op value
                        | StringOp (op, value) -> (|>) prefix >> Option.map JsonExtensions.AsString >> evaluateStringComparison op value
                    | Empty -> (fun context->true)

            
            CompileExpression "" exp

    let compile (settings: ParserSettings) (matcher: MatcherExpression) =
        //settings.Comparers
        let comparersMaps = (settings.Comparers |> toMap).Add("date", new ComparerDelegate(fun x -> DateTime.Parse(x) :> IComparable))
        (matcher |> (compile_internal comparersMaps))