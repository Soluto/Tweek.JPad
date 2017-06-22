module Tests.Common
open Tweek.JPad
open System.Security.Cryptography

[<AutoOpen>]
module Helpers = 
    let defaultSha1Provider = Sha1Provider( fun data ->
        let sha1 = SHA1.Create()
        sha1.ComputeHash data)
