module Tests.Common
open Tweek.JPad
open PCLCrypto

[<AutoOpen>]
module Helpers = 
    let defaultSha1Provider = Sha1Provider( fun data ->
        let sha1 = PCLCrypto.WinRTCrypto.HashAlgorithmProvider.OpenAlgorithm(HashAlgorithm.Sha1)
        sha1.HashData data)