namespace Tweek.JPad.Parsing
open Tweek.JPad.AST;
open System;
open FSharpUtils.Newtonsoft;
open Tweek.JPad

module public Matcher = 

    let private nullProtect x = match x with |null -> None |_-> Some x;

    let private (|KeyProperty|Operator|) (input:string) = if input.[0] = '$' then Operator else KeyProperty

    let private reducePredicate op seq = if Seq.isEmpty(seq) then true else seq |> Seq.reduce(op)               

    let private reduceOrElse reduceFun alt seq = if not (Seq.isEmpty(seq)) then seq|> Seq.reduce reduceFun else alt

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
        | _ -> ParseError("Invalid time unit") |> raise

                                                    
    let rec private parseOp comparisionType op jsonvalue : MatcherExpression = match op, jsonvalue with
        |"$not", _ -> Op(Op.Not ((parsePropertySchema And comparisionType jsonvalue)))
        |"$or", _ -> parsePropertySchema Or comparisionType jsonvalue
        |"$and", _ -> parsePropertySchema And comparisionType jsonvalue
        |"$ge", _ -> Op(CompareOp(GreaterEqual, jsonvalue, comparisionType))
        |"$eq", _ -> Op(CompareOp(Equal, jsonvalue, comparisionType))
        |"$gt", _ -> Op(CompareOp(GreaterThan, jsonvalue, comparisionType))
        |"$le", _ -> Op(CompareOp(LessEqual, jsonvalue, comparisionType))
        |"$lt", _ -> Op(CompareOp(LessThan, jsonvalue, comparisionType))
        |"$ne", _ -> Op(CompareOp(NotEqual, jsonvalue, comparisionType))
        |"$in", Array a -> Op(In(a, comparisionType))
        |"$contains", String s -> Op((StringOp(Contains, s)))
        |"$startsWith", String s -> Op(StringOp(StartsWith, s))
        |"$endsWith", String s -> Op(StringOp(EndsWith, s))
        |"$withinTime", String s -> Op(TimeOp(WithinTime, parseTimeUnit(s)))
        | s, _ -> raise (ParseError("expected operator, found:"+s))

    and private parsePropertySchema (conjuctionOp : ConjuctionOp)  (comparisonType:ComparisonType) (schema:JsonValue)  : MatcherExpression = 
        match schema with 
        | Record record -> 
            let converterType = record |> Seq.tryFind (fst >> (=) "$compare")
            let filter = (match converterType with |None -> id |Some x -> Seq.filter ((<>) x) )
            let newComparisonType = match converterType with 
                                        |Some (_, convertType) -> ComparisonType.Custom (convertType |> JsonExtensions.AsString)
                                        |None -> comparisonType
            record |> 
                filter |>
                Seq.map (fun (key,innerSchema)-> match key with 
                    |KeyProperty-> Property(key, innerSchema |> parsePropertySchema And newComparisonType)
                    |Operator -> parseOp newComparisonType key innerSchema
                ) |> reduceOrElse (fun acc exp-> Op(ConjuctionOp(conjuctionOp, acc, exp))) Empty
        | x -> Op(CompareOp(Equal,x, comparisonType))

    let parse (schema:JsonValue) = parsePropertySchema And Auto schema