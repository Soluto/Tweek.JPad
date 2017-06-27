module TypedValueDistribution

open FsUnit
open Xunit
open FsCheck.Xunit;
open FSharpUtils.Newtonsoft;
open Microsoft.FSharp.Reflection;
open Newtonsoft.Json
open Tweek.JPad
open FsCheck
open System
open Tests.Common

type ``TypedValueDistribution Tests`` ()=
    let parser = JPadParser(ParserSettings(defaultSha1Provider))
    let createContext seq = ContextDelegate(fun name -> seq |> Seq.tryFind (fun (k,v)->k = name) |> Option.map (fun (k,v)->JsonValue.String v))
    let validate (rules:JPadEvaluateExt) context value = rules.Invoke context |> should equal (Some(value))
    let context = createContext [("device.@@id","123");]

    [<Fact>]
    member test.``Use valueType with number typed weighted value distribution``() =
        let rules = parser.Parse """
        {
            "partitions": [],
            "valueType": "number",
            "rules": [
                {
                    "Salt": "678107bb-51de-46ee-b127-2672c2303a47",
                    "Matcher": {},
                    "Type": "MultiVariant",
                    "OwnerType": "device",
                    "ValueDistribution": {
                        "type": "weighted",
                        "args": {
                            "5": 0,
                            "6": 100
                        }
                    }
                }
            ]
        }"""
        validate rules context (JsonValue.Number 6M)

    [<Fact>]
    member test.``Use valueType with boolean typed weighted value distribution``() =
        let rules = parser.Parse """
        {
            "partitions": [],
            "valueType": "boolean",
            "rules": [
                {
                    "Id": "123",
                    "Matcher": {},
                    "Type": "MultiVariant",
                    "OwnerType": "device",
                    "ValueDistribution": {
                        "type": "weighted",
                        "args": {
                            "false": 0,
                            "true": 100
                        }
                    }
                }
            ]
        }"""
        validate rules context (JsonValue.Boolean true) 
     
    [<Fact>]
    member test.``Use valueType with string typed weighted value distribution``() =
        let rules = parser.Parse """
        {
            "partitions": [],
            "valueType": "string",
            "rules": [
                {
                    "Id": "123",
                    "Matcher": {},
                    "Type": "MultiVariant",
                    "OwnerType": "device",
                    "ValueDistribution": {
                        "type": "weighted",
                        "args": {
                            "false" : 0,
                            "true" : 100
                        }
                    }
                }
            ]
        }"""
        validate rules context (JsonValue.String "true")
     
    [<Fact>]
    member test.``value distribution should prefer Salt over Id``() =
        let rules = parser.Parse """
        {
            "partitions": [],
            "valueType": "string",
            "rules": [
                {
                    "Salt": "32123",
                    "Id": "123",
                    "Matcher": {},
                    "Type": "MultiVariant",
                    "OwnerType": "device",
                    "ValueDistribution": {
                        "type": "weighted",
                        "args": {
                            "false" : 50,
                            "true" : 50
                        }
                    }
                }
            ]
        }"""
        validate rules context (JsonValue.String "true")

    [<Fact>]
    member test.``value distribution with Salt should be the same as Id``() =
        let getRules prop salt = sprintf """{"partitions":[],"valueType":"string","rules":[{"%s":"%s","Matcher":{},"Type":"MultiVariant","OwnerType":"device","ValueDistribution":{"type":"weighted","args":{"false":50,"true":50}}}]}""" prop salt
        let salt = Guid.NewGuid().ToString()

        let saltRules = getRules "Salt" salt |> parser.Parse
        let idRules = getRules "Id" salt |> parser.Parse

        saltRules.Invoke context |> should equal (idRules.Invoke context)
