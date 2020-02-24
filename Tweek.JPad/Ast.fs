namespace Tweek.JPad
open FSharp.Data;
open System;
open FSharpUtils.Newtonsoft;

module AST = 
    type JPad = {
        Rules: RulesContainer
        ValueType: string
        DefaultValue: JsonValue option
    }
    and Partitions = string[]
    and DefaultPartition = RulesContainer
    and PartitionProperty = string
    and RulesContainer = 
        | RulesByPartition of PartitionProperty * Map<string,RulesContainer> * DefaultPartition
        | RulesList of (MatcherExpression * RuleValue) list
    and RuleSimpleValue = JsonValue
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
    and Op = 
        | CompareOp of CompareOp * ComparisonValue * ComparisonType
        | StringOp of StringOp * String
        | In of ComparisonValue[] * ComparisonType
        | TimeOp of TimeOp * TimeSpan
        | ContainsOp of ComparisonValue * ComparisonType
        | ConjuctionOp of ConjuctionOp * MatcherExpression * MatcherExpression
        | Not of MatcherExpression

    and UnaryOp = Not
    and StringOp = StartsWith | EndsWith
    and ComparisonValue = JsonValue
