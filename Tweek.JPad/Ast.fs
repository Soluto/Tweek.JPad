namespace Tweek.JPad
open FSharp.Data;
open System;
open FSharpUtils.Newtonsoft;

module AST = 
    type JPad = {
        Partitions:Partitions
        Rules: RulesContainer
        ValueType: string
        DefaultValue: JsonValue option
    }
    and Partitions = string[]
    and RulesContainer = 
        | RulesByPartition of PatternBlock[]
        | RulesList of (MatcherExpression * RuleValue) list
    and PatternBlock =
        | Map of Map<string,RulesContainer>
        | Patterns of (Pattern * RulesContainer) list
        | Default of RulesContainer
    and RuleSimpleValue = JsonValue
    and Pattern = string
    and PatternType = 
        | Exact
        | Pattern
        | Default
    and MatcherExpression = 
            | Property of PropertyName * MatcherExpression
            | Not of MatcherExpression
            | Conjuction of ConjuctionOp * MatcherExpression * MatcherExpression
            | Binary of BinaryOp * ComparisonType * ComparisonValue
            | Empty
    and ComparisonType = | Auto
                         | Custom of String
    and RuleValue = 
        | SingleVariant of JsonValue
        | MultiVariant  of ValueDistribution
    and ValueDistribution = {
            DistributionType: DistributionType
            OwnerType: Option<string>
            Salt:string
        }
    and DistributionType = 
        | Uniform  of JsonValue[]
        | Weighted of (JsonValue * int)[]
        | Bernouli of float
    and PropertyName = string
    and ConjuctionOp = And | Or 
    and CompareOp = Equal | GreaterThan | LessThan | GreaterEqual | LessEqual | NotEqual 
    and TimeOp = WithinTime
    and Op = 
        | BinaryOp of BinaryOp
        | ConjuctionOp of ConjuctionOp
        | Not
    and BinaryOp = 
        | CompareOp of CompareOp
        | StringOp of StringOp
        | ContainsOp
        | AnyOp
        | AllOp
        | IsOp
        | In
        | TimeOp of TimeOp
    and UnaryOp = Not
    and StringOp = StartsWith | EndsWith
    and ComparisonValue = JsonValue
