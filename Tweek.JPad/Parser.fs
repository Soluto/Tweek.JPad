namespace Tweek.JPad
open FSharp.Data
open System;
open Tweek.JPad.AST;
open FSharpUtils.Newtonsoft;

type public JPadParser(settings:ParserSettings) = 
    member public __.BuildAST : (string -> JPad) = 
        JsonValue.Parse >>
        Parsing.Tree.parse
        
    member public __.Parse : (string -> JPadEvaluateExt) = 
        JsonValue.Parse >>
        Parsing.Tree.parse >>
        Compilation.Tree.compile settings
    
