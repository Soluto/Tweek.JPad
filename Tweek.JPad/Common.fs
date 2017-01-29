namespace Tweek.JPad
open System;
open FSharpUtils.Newtonsoft;
open PCLCrypto;

type ComparerDelegate = delegate of string -> IComparable
type Context = string-> Option<JsonValue>
type ContextDelegate = delegate of string -> Option<JsonValue>
type Sha1Provider = delegate of byte[] -> byte[]

[<AutoOpen>]
module Helpers = 
    let defaultSha1Provider = Sha1Provider( fun data ->
        let sha1 = PCLCrypto.WinRTCrypto.HashAlgorithmProvider.OpenAlgorithm(HashAlgorithm.Sha1)
        sha1.HashData data)

type ParserSettings(?Comparers: System.Collections.Generic.IDictionary<string,ComparerDelegate>, ?Sha1Provider:Sha1Provider) = 
  member x.Comparers = defaultArg Comparers (dict([]))
  member x.Sha1Provider = defaultArg Sha1Provider defaultSha1Provider

exception ParseError of string
type JPadEvaluate = Context -> Option<JsonValue>