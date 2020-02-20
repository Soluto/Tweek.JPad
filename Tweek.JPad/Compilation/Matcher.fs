namespace Tweek.JPad.Compilation
open Tweek.JPad.AST;
open System;
open FSharpUtils.Newtonsoft;
open Tweek.JPad

module public Matcher = 
    let inline isNull value = obj.ReferenceEquals(value, null)

    let falseOnNone fn opt = match opt with |Some v -> if isNull v then false else fn v 
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
            | String stringValue -> stringValue |> DateTime.Parse |> Some
            | Null -> None
            | _ -> Exception("Invalid time format") |> raise

    let inline private evaluateComparisonOp op a b = 
                    match op with
                    | Equal -> a = b
                    | GreaterThan -> a < b
                    | LessThan -> a > b
                    | GreaterEqual ->  a <= b
                    | LessEqual -> a >= b
                    | NotEqual -> a <> b

    let private evaluateComparison (comparer) (op: CompareOp) (leftValue:ComparisonValue) =
        match (leftValue) with
            | String l -> (comparer l) |> (fun comparison rightValue -> 
                match rightValue with 
                | None -> false 
                | Some json -> 
                    match json with 
                    |String v -> evaluateComparisonOp op (comparison v) 0
                    | _ -> false)
            | _ -> (fun (rightValueOption:Option<ComparisonValue>) -> 
                match (leftValue, op, rightValueOption) with
                | Null, Equal, None -> true
                | Null, _, None -> false
                | _, NotEqual, None -> true
                | _, _, None -> false
                | _, _, Some rightValue -> 
                            match (leftValue, op,  rightValue) with
                            | Null, Equal, Null -> true
                            | Null, _, Null -> false
                            | _, NotEqual, Null -> true
                            | Null, NotEqual, _ -> true
                            | _, _, Null -> false
                            | Null, _, _ -> false
                            | Number x, _ , Number y -> evaluateComparisonOp op x y
                            | Number x, _ , Float y -> evaluateComparisonOp op x (decimal y)
                            | Number x, _ , String y -> decimal y |> evaluateComparisonOp op x 
                            | Boolean x, _ ,Boolean y -> evaluateComparisonOp op x y
                            | Boolean x, _ ,String y -> bool.Parse y |> evaluateComparisonOp op x
                            | Float x, _ ,Float y -> evaluateComparisonOp op x y
                            | Float x, _ ,Number y -> evaluateComparisonOp op (decimal x) y
                            | Float x, _ ,String y -> float y |> evaluateComparisonOp op x
                            | _ , _ , _ -> Exception("non matching types") |> raise
                        )

    let private evaluateStringComparison (op: StringOp) (leftValue:string) =
        let casedValue = leftValue.ToLower()
        match (op) with
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

    let private evaluateContains (comparer) (leftValue:ComparisonValue) (rightValue:JsonValue) =    
        let arrayExist item array = array |> Array.exists (fun t->  (evaluateComparison comparer CompareOp.Equal item (Some t)))
        
        match (leftValue, rightValue) with 
            | String l, String r -> r.ToLower().Contains(l.ToLower())
            | Array l, Array r -> l |> Array.forall (fun i -> arrayExist i r)
            | _, Array r -> arrayExist leftValue  r
            | Array l, _ -> Array.length l = 1 &&  arrayExist rightValue l
            | _, _ -> false

    let private compile_internal (comparers:Map<string,ComparerDelegate>) exp  = 
            let defaultComparer (l:string) (r:string) = l.ToLower().CompareTo (r.ToLower())
            let getComparer comparisonType = match comparisonType with
                                             | Auto -> defaultComparer
                                             | Custom s -> if comparers.ContainsKey(s) then createComparer(comparers.[s].Invoke) else ParseError("missing comparer - " + s) |> raise

            let rec CompileExpression (prefix:string) (exp: MatcherExpression) : (Context) -> bool =
                match exp with
                    | Property (prop, innerexp) -> CompileExpression (getPropName prefix prop) innerexp 
                    | Op o -> 
                        match o with
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
                        | ContainsOp (value, newComparisonType) -> (|>) prefix >> (evaluateContains (getComparer newComparisonType) value |> falseOnNone) 
                    | Empty -> (fun context->true)

            
            CompileExpression "" exp

    let compile (settings: ParserSettings) (matcher: MatcherExpression) =
        //settings.Comparers
        let comparersMaps = (settings.Comparers |> toMap).Add("date", new ComparerDelegate(fun x -> DateTime.Parse(x) :> IComparable))
        (matcher |> (compile_internal comparersMaps))