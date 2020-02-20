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
            | Op of Op
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
    and NestedOp = All | Is | Any
    and Op = 
        | CompareOp of CompareOp * ComparisonValue * ComparisonType
        | StringOp of StringOp * String
        | In of ComparisonValue[] * ComparisonType
        | TimeOp of TimeOp * TimeSpan
        | ContainsOp of ComparisonValue * ComparisonType
        | ConjuctionOp of ConjuctionOp * MatcherExpression * MatcherExpression
        | Not of MatcherExpression
        | NestedOp of NestedOp * MatcherExpression

    and StringOp = StartsWith | EndsWith
    and ComparisonValue = JsonValue
