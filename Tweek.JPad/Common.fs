namespace Tweek.JPad
open System;
open FSharpUtils.Newtonsoft;

type ComparerDelegate = delegate of string -> IComparable
type Context = string-> Option<JsonValue>
type ContextDelegate = delegate of string -> Option<JsonValue>
type Sha1Provider = delegate of byte[] -> byte[]
type public JPadEvaluateExt = delegate of ContextDelegate -> Option<JsonValue>

type ParserSettings(sha1Provider:Sha1Provider, ?Comparers: System.Collections.Generic.IDictionary<string,ComparerDelegate>) = 
  member x.Comparers = defaultArg Comparers (dict([]))
  member x.Sha1Provider = sha1Provider

exception ParseError of string
type JPadEvaluate = Context -> Option<JsonValue>