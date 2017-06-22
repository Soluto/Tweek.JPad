module ValueDistributor.Tests.Main

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

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

type ``ValueDistributor tests`` () =
    let generatedCalculatedScheme weights = weights |> Array.mapi (fun a b -> (a,b) )
                                                    |> dict 
                                                    |> JsonConvert.SerializeObject 
                                                    |> sprintf """{"type": "weighted","args": %s }""" 
                                                    |> JsonValue.Parse
                                                    |> ValueDistribution.parse "string"
                                                    |> ValueDistribution.compile

    let assertCalculated (weights:float[]) (numberOfUsers:int) (samplingError:float) (calcFunction:obj[]->JsonValue)  = 
                let sumWeights = weights |> Array.sum
                let rnd = new Random();
                [|1..numberOfUsers|] 
                            |> Seq.map (fun _-> rnd.Next(10000,100000))
                            |> Seq.map (fun x-> calcFunction([|x|]))
                            |> Seq.countBy id
                            |> Seq.sortBy fst 
                            |> Seq.zip weights
                            |> Seq.map (fun (expectedWeight, (_,actualWeight) )->  
                            ( ((float actualWeight)/(float numberOfUsers)), (expectedWeight/sumWeights) )) 
                            |> Seq.iter (fun (expected, actual ) ->
                            expected |> should (equalWithin samplingError) actual)

    [<Fact>]
    member test.``Use uniform distrubtion with single value``() =
        let calculator = """{"type": "uniform", "args": ["abc"] }""" |> JsonValue.Parse |> ValueDistribution.parse "string"  |> ValueDistribution.compile
        calculator defaultSha1Provider [|"userName", 5|]  |> should equal (JsonValue.String "abc");
        
    [<Fact>]
    member test.``Use weighted distrubtion with single value``() =
        let calculator = """{"type": "weighted","args": {"5": 1} }""" |> JsonValue.Parse |>  ValueDistribution.parse "string" |> ValueDistribution.compile
        calculator defaultSha1Provider [|"userName", 5|]  |> should equal (JsonValue.String "5");


    [<Fact>]
    member test.``Use weighted distrubtion with numeric value``() =
        let calculator = """{"type": "weighted","args": {"5": 1} }""" |> JsonValue.Parse |> ValueDistribution.parse "number"  |> ValueDistribution.compile
        calculator defaultSha1Provider [|"userName", 5|]  |> should equal (JsonValue.Number 5M);

    [<Fact>]
    member test.``Use weighted distrubtion with boolean value``() =
        let calculator = """{"type": "weighted","args": {"true": 1} }""" |> JsonValue.Parse |> ValueDistribution.parse "boolean"  |> ValueDistribution.compile
        calculator defaultSha1Provider [|"userName", 5|]  |> should equal (JsonValue.Boolean true);

        
    [<Fact>]
    member test.``Use weighted distrubtion with unknown value``() =
        let calculator = """{"type": "weighted","args": {"true": 1} }""" |> JsonValue.Parse |> ValueDistribution.parse "otherType"  |> ValueDistribution.compile
        calculator defaultSha1Provider [|"userName", 5|]  |> should equal (JsonValue.String "true");


    [<Property>]
    member test.``Use Bernoulli distribution should equal weighted``() =
        let generator = Gen.elements([0.01..0.99]) |> Arb.fromGen
        Prop.forAll generator (fun p -> 
            let q = 1.0-p;
            let weightedInput = (sprintf """{"type": "weighted","args": {"true": %d, "false": %d} }""" (p*100.0 |> int ) (q*100.0 |> int))
            let bernoulliInput = (sprintf """{"type": "bernoulliTrial","args": %.2f }""" p)
            let calculatorWeighted = (weightedInput |> JsonValue.Parse |>  ValueDistribution.parse "string" |> ValueDistribution.compile) defaultSha1Provider
            let calculatorBernoulli = (bernoulliInput |> JsonValue.Parse |>  ValueDistribution.parse "string" |> ValueDistribution.compile) defaultSha1Provider
            let getValue x = match x with | JsonValue.String "true" -> 1 | JsonValue.String "false" -> 0 | JsonValue.Boolean true -> 1 | JsonValue.Boolean false -> 0 
            let numTests = 1000;
            [|1..numTests|]
                |> Seq.map (fun x -> (calculatorWeighted [|x|], calculatorBernoulli [|x|]))
                |> Seq.fold (fun (accWeighted, accBernoulli) (nextWeighted, nextBernoulli) -> (accWeighted + (getValue nextWeighted), accBernoulli + (getValue nextBernoulli))) (0, 0)
                |> fun (weightedResult, bernoulliResult) -> weightedResult |> should (equalWithin (numTests/20)) bernoulliResult
        )

    [<Fact>]
    member test.``run single tests and verify similar values``()=
        let weights = [|1.0;5.0;6.0|]
        let calculatorWeighted = (generatedCalculatedScheme weights) defaultSha1Provider
        let totalUsers = 100000
        let samplingError = 0.01
        assertCalculated weights totalUsers samplingError calculatorWeighted

    [<Property>]
    member test.``run many tests and verify similar values``()=
        let gen = Gen.sequence ([Gen.elements([1..10]);
                              Gen.elements([1..10]);
                              Gen.elements([1..10]);
                              ]) |> Arb.fromGen

        let totalUsers = 1000
        let samplingError = 0.06
        Prop.forAll gen (fun test -> 
                            let weights = test |> Seq.map float |> Seq.toArray
                            let calculatorWeighted = (generatedCalculatedScheme weights) defaultSha1Provider
                            assertCalculated weights totalUsers samplingError calculatorWeighted
        )

    [<Fact>]
    member test.``FF rollout is possible with Bernoulli ``()=
        let rnd = new Random();
        let users = [|1..100|] |> Array.map (fun _-> rnd.Next (10000,100000));
        let getCalculator = (sprintf """{"type": "bernoulliTrial","args": %f }""") >> JsonValue.Parse >> ValueDistribution.parse "string" >> ValueDistribution.compile

        [|1..20|] |> Array.map ((*) 5)
                  |> Array.map (fun i-> 
                        let calc = (getCalculator ((float i)/100.0)) defaultSha1Provider
                        users |> Array.filter (fun x-> calc [|x|] = JsonValue.Boolean(true))
                  )
                  |> Array.pairwise
                  |> Array.iter (fun (prev,next) ->
                             prev
                             |> Array.forall (fun x-> next |> Array.contains x)
                             |> should equal true)
