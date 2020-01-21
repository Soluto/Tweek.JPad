module Matcher.Tests.Main

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

open FsUnit
open Xunit
open FSharpUtils.Newtonsoft;
open Tweek.JPad;
open System;

type ``Matcher tests`` ()=
    let validator jsonString = Matcher.createEvaluator (dict[]) (jsonString|>JsonValue.Parse|>Matcher.parse)
    let validatorWithComparers jsonString comparers = Matcher.createEvaluator comparers (jsonString|>JsonValue.Parse|>Matcher.parse)
    let createContext seq  =  fun (name:string) -> seq |> Seq.tryFind (fun (k,v)-> k = name) |> Option.map (fun (k,v)-> v)
    let context = createContext;

    [<Fact>] 
    member test.``Use multipe-comparisons, "and" is implict``() =
        let validate = validator """{"Age": {"$le":30, "$ge":25} }"""

        validate (context [("Age", JsonValue.Number(30m));])  |> should equal true;
        validate (context [("Age", JsonValue.Number(26m));])  |> should equal true;
        validate (context [("Age", JsonValue.Number(31m));])  |> should equal false;
        validate (context [("Age", JsonValue.Number(24m));])  |> should equal false;

    [<Fact>]
    member test.``Use comparisons on multipe fields, "and" is implict``() =
        let validate = validator """{"Age": 20, "Weight": 40 }"""
        validate (context [("Age", JsonValue.Number(20m));("Weight", JsonValue.Number(40m))])  |> should equal true;
        validate (context [("Age", JsonValue.Number(20m));("Weight", JsonValue.Number(39m))])  |> should equal false;
        validate (context [("Age", JsonValue.Number(19m));("Weight", JsonValue.Number(40m))])  |> should equal false;

    [<Fact>]
    member test.``Use not ``() =
        let validate = validator """{"$not":{"Age": {"$lt":21}}}"""
        validate (context [("Age", JsonValue.Number(22m));])  |> should equal true;
        validate (context [("Age", JsonValue.Number(20m));])  |> should equal false;
        
    [<Fact>]
    member test.``Use logical operater at root``() =
        let validate = validator """{"$or": {"Age": {"$gt" : 20, "$lt" : 23 } , "Weight": {"$lt":80}} }""";
        validate (context [("Age", JsonValue.Number(25m));("Weight", JsonValue.Number(70m))]) |> should equal true;
        validate (context [("Age", JsonValue.Number(25m));("Weight", JsonValue.Number(80m))]) |> should equal false;
        validate (context [("Age", JsonValue.Number(22m));("Weight", JsonValue.Number(70m))]) |> should equal true;
        validate (context [("Age", JsonValue.Number(22m));("Weight", JsonValue.Number(80m))]) |> should equal true;

    [<Fact>]
    member test.``"nested" context``() =
        let validate = validator """{"Person": {"Age": 25 }}""";
        validate (context [("Person.Age", JsonValue.Number(25m))]) |> should equal true;

    [<Fact>]
    member test.``use custom comparer``() =
        let comparers = dict([("version", new ComparerDelegate(fun x -> Version.Parse(x) :> IComparable))]);
        let matcher = """{"AgentVersion": {"$compare": "version", "$gt": "1.5.1", "$le": "1.15.2" }}""" |> JsonValue.Parse |> Matcher.parse;
        let validate =  Matcher.createEvaluator comparers matcher;
        validate (context [("AgentVersion", JsonValue.String("1.15.1"))]) |> should equal true;

    [<Fact>]
    member test.``use custom comparer with broken mismatched target value should fail in compile time``() =
        let comparers = dict([("version", new ComparerDelegate(fun x -> Version.Parse(x) :> IComparable))]);
        let matcher = """{"AgentVersion": {"$compare": "version", "$gt": "debug-1.5.1", "$le": "1.15.2" }}""" |> JsonValue.Parse |> Matcher.parse;
        (fun () ->Matcher.createEvaluator comparers matcher |> ignore) |> should throw typeof<ParseError>

    [<Fact>]
    member test.``exist/not exist prop support -> expressed with null``() =
        let validate = validator """{"Person": {"Age": null }}""";
        validate (context [("Person.Age", JsonValue.Number(20m))]) |> should equal false;
        validate (context [("abc", JsonValue.Number(30m))]) |> should equal true;
        validate (context []) |> should equal true;
        let validateWithNot = validator """{"Person": {"Age": {"$not": null} }}""";
        validateWithNot (context [("Person.Age", JsonValue.Number(20m))]) |> should equal true;
        validateWithNot (context [("abc", JsonValue.Number(30m))]) |> should equal false;
        validateWithNot (context []) |> should equal false;


    [<Fact>]
    member test.``in operator support ``() =
        let validate = validator """{"Person": {"Age": {"$in" :[10,20,30]}}}""";
        validate (context [("Person.Age", JsonValue.Number(20m))]) |> should equal true;
        validate (context [("Person.Age", JsonValue.Number(21m))]) |> should equal false;
        validate (context [("Person.Age", JsonValue.Number(100m))]) |> should equal false;
        validate (context [("Person.Age", JsonValue.Number(10m))]) |> should equal true;


    [<Fact>]
    member test.``Use Equal``() =
        let validate = validator """{"Age": {"$eq": 30 }}"""
        validate (context [("Age", JsonValue.Number(31m));])  |> should equal false
        validate (context [("Age", JsonValue.Number(30m));])  |> should equal true
        validate (context [("Age", JsonValue.Number(29m));])  |> should equal false
        validate (context [("Age", JsonValue.Null);]) |> should equal false

    [<Fact>]
    member test.``Use greaterEqual``() =
        let validate = validator """{"Age": {"$ge": 30 }}"""
        validate (context [("Age", JsonValue.Number(31m));])  |> should equal true
        validate (context [("Age", JsonValue.Number(30m));])  |> should equal true
        validate (context [("Age", JsonValue.Number(29m));])  |> should equal false
        validate (context [("Age", JsonValue.Null);]) |> should equal false

    [<Fact>]
    member test.``Use lessEqual``() =
        let validate = validator """{"Age": {"$le": 30 }}"""
        validate (context [("Age", JsonValue.Number(31m));])  |> should equal false
        validate (context [("Age", JsonValue.Number(30m));])  |> should equal true
        validate (context [("Age", JsonValue.Number(29m));])  |> should equal true
        validate (context [("Age", JsonValue.Null);]) |> should equal false

    [<Fact>]
    member test.``Use lessThanOp``() =
        let validate = validator """{"Age": {"$lt": 30 }}"""
        validate (context [("Age", JsonValue.Number(31m));])  |> should equal false
        validate (context [("Age", JsonValue.Number(30m));])  |> should equal false
        validate (context [("Age", JsonValue.Number(29m));])  |> should equal true
        validate (context [("Age", JsonValue.Null);]) |> should equal false

    [<Fact>]
    member test.``Use greaterThenOp``() =
        let validate = validator """{"Age": {"$gt": 30 }}"""
        validate (context [("Age", JsonValue.Number(31m));])  |> should equal true
        validate (context [("Age", JsonValue.Number(30m));])  |> should equal false
        validate (context [("Age", JsonValue.Number(29m));])  |> should equal false
        validate (context [("Age", JsonValue.Null);]) |> should equal false

    [<Fact>]
    member test.``Use implict Equal``() =
        let validate = validator """{"Age": 30 }"""
        let explictValidate = validator """{"Age": {"$eq": 30 } }"""
        let compareValidators ctx = (validate ctx) |> should equal (explictValidate ctx)
        compareValidators (context [("Age", JsonValue.Number(31m));]) 
        compareValidators (context [("Age", JsonValue.Number(30m));]) 
        compareValidators (context [("Age", JsonValue.Number(29m));]) 
        compareValidators (context [("Age", JsonValue.Null);])

    [<Fact>]
    member test.``Compare numeric values``() =
        let validate = validator """{"Age": 30}"""
        validate (context [("Age", JsonValue.Number(30m));])  |> should equal true
        validate (context [("Age", JsonValue.Number(31m));])  |> should equal false

    [<Fact>]
    member test.``Compare string values``() =
        let validate = validator """{"Country": "Germany"}"""
        validate (context [("Country", JsonValue.String("Germany"));])  |> should equal true
        validate (context [("Country", JsonValue.String("France"));])  |> should equal false

    [<Fact>]
    member test.``Compare boolean values``() =
        let validate = validator """{"IsVIP": true}"""
        validate (context [("IsVIP", JsonValue.Boolean(true));])  |> should equal true
        validate (context [("IsVIP", JsonValue.Boolean(false));])  |> should equal false

    [<Fact>]
    member test.``Use null value in rule``() =
        let validate = validator """{"GroupName": null}"""
        validate (context [("GroupName", JsonValue.String("Some Group"));])  |> should equal false
        validate (context [("GroupName", JsonValue.Null);])  |> should equal true
        validate (context [])  |> should equal true

    [<Fact>]
    member test.``Compare to null value from context``() =
        let validate = validator """{"GroupName": "Some Group"}"""
        validate (context [("GroupName", JsonValue.Null);])  |> should equal false
        validate (context [])  |> should equal false

    [<Fact>]
    member test.``Compare to null value with not equal operator``() =
        let validate = validator """{"Age": {"$ne": null }}"""
        validate (context [("Age", JsonValue.Number(30m));])  |> should equal true
        validate (context [("Age", JsonValue.Null);])  |> should equal false
        validate (context [])  |> should equal false

    [<Fact>]
    member test.``Compare incompatible values``() =
        let validate = validator """{"Age": 30}"""
        (fun () -> validate (context [("Age", JsonValue.Boolean(false));]) |> ignore) |> should throw typeof<Exception>
        
    [<Fact>]
    member test.``DateCompare using withinTime with days``() =
        let validate = validator """{"Birthday": {"$withinTime": "10d"}}"""
        validate (context [("Birthday", JsonValue.String(DateTime.UtcNow.AddDays(-20.0).ToString()));("system.time_utc", JsonValue.String(DateTime.UtcNow.ToString()));])  |> should equal false
        validate (context [("Birthday", JsonValue.String(DateTime.UtcNow.AddDays(-5.0).ToString()));("system.time_utc", JsonValue.String(DateTime.UtcNow.ToString()));])  |> should equal true
        validate (context [])  |> should equal false
        validate (context [("Birthday", JsonValue.Null);]) |> should equal false

    [<Fact>]
    member test.``DateCompare using withinTime with hours``() =
        let validate = validator """{"Birthday": {"$withinTime": "10h"}}"""
        validate (context [("Birthday", JsonValue.String(DateTime.UtcNow.AddHours(-20.0).ToString()));("system.time_utc", JsonValue.String(DateTime.UtcNow.ToString()));])  |> should equal false
        validate (context [("Birthday", JsonValue.String(DateTime.UtcNow.AddHours(-5.0).ToString()));("system.time_utc", JsonValue.String(DateTime.UtcNow.ToString()));])  |> should equal true
        validate (context [])  |> should equal false
        validate (context [("Birthday", JsonValue.Null);]) |> should equal false

    [<Fact>]
    member test.``DateCompare using withinTime with minutes``() =
        let validate = validator """{"Birthday": {"$withinTime": "10m"}}"""
        validate (context [("Birthday", JsonValue.String(DateTime.UtcNow.AddMinutes(-20.0).ToString()));("system.time_utc", JsonValue.String(DateTime.UtcNow.ToString()));])  |> should equal false
        validate (context [("Birthday", JsonValue.String(DateTime.UtcNow.AddMinutes(-5.0).ToString()));("system.time_utc", JsonValue.String(DateTime.UtcNow.ToString()));])  |> should equal true
        validate (context [])  |> should equal false
        validate (context [("Birthday", JsonValue.Null);]) |> should equal false

    [<Fact>]
    member test.``DateCompare using withinTime with invalid time unit format``() =
        (fun () -> validator """{"Birthday": {"$withinTime": "10z"}}""" |> ignore) |> should throw typeof<Exception>
        (fun () -> validator """{"Birthday": {"$withinTime": null}}""" |> ignore) |> should throw typeof<Exception>
        (fun () -> validator """{"Birthday": {"$withinTime": "a long long time ago"}}""" |> ignore) |> should throw typeof<Exception>

    [<Fact>]
    member test.``DateCompare with string comparer``() =
        let validate = validator """{"Birthday": {"$ge": "2014-12-20T13:14:19.790Z", "$compare": "date"}}"""
        validate (context [("Birthday", JsonValue.String("2015-12-20T13:14:19.790Z"));] ) |> should equal true
        validate (context [("Birthday", JsonValue.String("2013-12-20T13:14:19.790Z"));] ) |> should equal false
        validate (context [("Birthday", JsonValue.Null);]) |> should equal false
    
    [<Fact>]
    member test.``DateCompare should fail to compile matcher with an invalid date``() =
        (fun () -> validator """{"Birthday": {"$gt": "aa2014-12-20T13:14:19.790Z", "$compare": "date"}}""" |> ignore) |> should throw typeof<ParseError>

    [<Fact>]
    member test.``Array comparers - contains``() =
        let validate = validator """{"Countries": {"$contains": "AustRalia" }}"""
        let validateEmpty = validator """{"Countries": {"$contains": "" }}"""
        let validateList= validator """{"Countries": {"$contains": ["israel","iTaly"] }}"""
        let validateNumberList= validator """{"Codes": {"$contains": [1] }}"""
        let validateSingleList= validator """{"Countries": {"$contains": ["israel"] }}"""
        let validateEmptyList = validator """{"Countries": {"$contains": [] }}"""
        let contries1 = [|JsonValue.String("IsrAel");JsonValue.String("Italy");JsonValue.String("Australia")|]
        let contries2 = [|JsonValue.String("IsrAel");JsonValue.String("fRance");JsonValue.String("GermaNy");JsonValue.String("iReland")|]
        let codes1 = [|JsonValue.Number(1m);JsonValue.Number(2m);JsonValue.Number(3m)|]
        let noCountries = [||]
        validate (context [("Countries", JsonValue.Array(contries1));])  |> should equal true
        validate (context [("Countries", JsonValue.Array(contries2));])  |> should equal false
        validate (context [("Countries", JsonValue.Array(noCountries));])  |> should equal false
        validate (context [("Countries", JsonValue.Null);])  |> should equal false
        validateEmpty (context [("Countries", JsonValue.Array(contries1));])  |> should equal false
        validateEmpty (context [("Countries", JsonValue.Array(noCountries));])  |> should equal false
        validateEmpty (context [("Countries", JsonValue.Null);])  |> should equal false
        validateList (context [("Countries", JsonValue.Array(contries1));])  |> should equal true
        validateList (context [("Countries", JsonValue.Array(contries2));])  |> should equal false
        validateList (context [("Countries", JsonValue.Array(noCountries));])  |> should equal false
        validateList (context [("Countries", JsonValue.String("IsrAel"));])  |> should equal false
        validateList (context [("Countries", JsonValue.Null);])  |> should equal false
        validateSingleList (context [("Countries", JsonValue.String("IsrAel"));])  |> should equal true
        validateSingleList (context [("Countries", JsonValue.String("Isrel"));])  |> should equal false
        validateSingleList (context [("Countries", JsonValue.Null);])  |> should equal false
        validateEmptyList (context [("Countries", JsonValue.Array(contries1));])  |> should equal true
        validateEmptyList (context [("Countries", JsonValue.Array(noCountries));])  |> should equal true
        validateEmptyList (context [("Countries", JsonValue.Null);])  |> should equal false
        validateNumberList (context [("Codes", JsonValue.Array(codes1));])  |> should equal true
        validateNumberList (context [("Codes", JsonValue.Null);])  |> should equal false

    [<Fact>]
    member test.``String comparers - contains``() =
        let validate = validator """{"Country": {"$contains": "ra" }}"""
        validate (context [("Country", JsonValue.String("Australia"));])  |> should equal true
        validate (context [("Country", JsonValue.String("IsrAel"));])  |> should equal true
        validate (context [("Country", JsonValue.String("Italy"));])  |> should equal false

    [<Fact>]
    member test.``String comparers - endsWith``() =
        let validate = validator """{"Country": {"$endsWith": "land" }}"""
        validate (context [("Country", JsonValue.String("Finland"));])  |> should equal true
        validate (context [("Country", JsonValue.String("EnglaND"));])  |> should equal true
        validate (context [("Country", JsonValue.String("Norway"));])  |> should equal false

    [<Fact>]
    member test.``String comparers - startsWith``() =
        let validate = validator """{"Country": {"$startsWith": "united" }}"""
        validate (context [("Country", JsonValue.String("United Stated"));])  |> should equal true
        validate (context [("Country", JsonValue.String("United Kingdom"));])  |> should equal true
        validate (context [("Country", JsonValue.String("Russia"));])  |> should equal false

    [<Fact>]
    member test.``String comparers - invalid comparison value``() =
        (fun () -> validator """{"Country": {"$startsWith": null }}""" |> ignore) |> should throw typeof<ParseError>

    [<Fact>]
    member test.``String comparers - null handling``() =
        let validate = validator """{"Country": {"$startsWith": "united" }}"""
        validate (context [||])  |> should equal false
        validate (context [("Country", JsonValue.Null);])  |> should equal false
        validate (context [("Country", JsonValue.String(null));])  |> should equal false
        
    [<Fact>]
    member test.``Array comparers - any``() =
        let countries = JsonValue.Parse """[{"name":"IsrAel"},{"name":"Italy"},{"name":"Australia"}]"""
        let validate = validator """{"Countries": {"$any": { "name": "Italy" } }}"""
        let validateNotExist = validator """{"Countries": {"$any": { "name": "Spain" } }}"""
        let validateOp = validator """{"Countries": {"$any": { "name": {"$contains": "ita"} } }}"""
        let validateOpNotExists = validator """{"Countries": {"$any": { "name": {"$contains": "xxx"} } }}"""
        let validatePrimitive = validator """{"Countries": {"$any": "Italy" }}"""
        validate (context [("Countries", countries);])  |> should equal true
        validateNotExist (context [("Countries", countries);])  |> should equal false
        validateOp (context [("Countries", countries);])  |> should equal true
        validateOpNotExists (context [("Countries", countries);])  |> should equal false
        validatePrimitive (context [("Countries", countries);])  |> should equal false
        validate (context [("Countries", JsonValue.Array([||]));])  |> should equal false
        validate (context [("Countries", JsonValue.Null);])  |> should equal false
        validate (context [("Countries", JsonValue.String(""));])  |> should equal false
        validate (context [("Countries", JsonValue.Boolean(false));])  |> should equal false
        
    [<Fact>]
    member test.``Array comparers - any with comparers``() =
        let devices = JsonValue.Parse """[{"name":"iPhone","AgentVersion":"0.1.1"},{"name":"Samsung","AgentVersion":"0.1.0"}]"""
        let comparers = dict([("version", new ComparerDelegate(fun x -> Version.Parse(x) :> IComparable))])
        let validate = validatorWithComparers """{"Devices": {"$any": {"AgentVersion": {"$compare": "version", "$gt": "0.1.0" }} }}""" comparers
        let validateNotExist = validatorWithComparers """{"Devices": {"$any": {"AgentVersion": {"$compare": "version", "$gt": "0.3.0" }} }}""" comparers   
        validate (context [("Devices", devices);])  |> should equal true
        validateNotExist (context [("Devices", devices);])  |> should equal false
        
    [<Fact>]
    member test.``Array comparers - any primitives``() =
        let ages = JsonValue.Parse """[35,40,18]"""
        let validate = validator """{"Ages": {"$any": 35}}"""
        let validateNotExist = validator """{"Ages": {"$any": 45}}"""
        let validateOp = validator """{"Ages": {"$any": { "$gt": 10 }}}"""
        let validateOpNotExist = validator """{"Ages": {"$any": { "$lt": 10 }}}"""
        validate (context [("Ages", ages);])  |> should equal true
        validateNotExist (context [("Ages", ages);])  |> should equal false
        validateOp (context [("Ages", ages);])  |> should equal true
        validateOpNotExist (context [("Ages", ages);])  |> should equal false

    [<Fact>]
    member test.``Array comparers - all``() =
        let countries = JsonValue.Parse """[{"name":"Canada","countryCode":1},{"name":"US","countryCode":1}]"""
        let validate = validator """{"Countries": {"$all": { "countryCode":1 } }}"""
        let validateNotExists = validator """{"Countries": {"$all": { "name": "Canada" } }}"""
        let validateOp = validator """{"Countries": {"$all": { "countryCode": { "$lt":2, "$gt":0 } } }}"""
        let validateOpNotExists = validator """{"Countries": {"$all": { "countryCode": { "$lt":1, "$gt":0 } } }}"""
        let validatePrimitive = validator """{"Countries": { "$all": 1 }}"""
        validate (context [("Countries", countries);])  |> should equal true
        validateNotExists (context [("Countries", countries);])  |> should equal false
        validateOp (context [("Countries", countries);])  |> should equal true
        validateOpNotExists (context [("Countries", countries);])  |> should equal false
        validatePrimitive (context [("Countries", countries);])  |> should equal false
        validate (context [("Countries", JsonValue.Array([||]));])  |> should equal true
        validate (context [("Countries", JsonValue.Null);])  |> should equal false
        validate (context [("Countries", JsonValue.String(""));])  |> should equal false
        validate (context [("Countries", JsonValue.Boolean(false));])  |> should equal false

    [<Fact>]
    member test.``Array comparers - all with comparers``() =
        let devices = JsonValue.Parse """[{"name":"iPhone","AgentVersion":"0.1.1"},{"name":"Samsung","AgentVersion":"0.1.2"}]"""
        let comparers = dict([("version", new ComparerDelegate(fun x -> Version.Parse(x) :> IComparable))])
        let validate = validatorWithComparers """{"Devices": {"$all": {"AgentVersion": {"$compare": "version", "$gt": "0.1.0" }} }}""" comparers
        let validateNotExist = validatorWithComparers """{"Devices": {"all": {"AgentVersion": {"$compare": "version", "$gt": "0.3.0" }} }}""" comparers   
        validate (context [("Devices", devices);])  |> should equal true
        validateNotExist (context [("Devices", devices);])  |> should equal false
        
    [<Fact>]
    member test.``Array comparers - all primitives``() =
        let ages = JsonValue.Parse """[35,35,35]"""
        let validate = validator """{"Ages": {"$all": 35}}"""
        let validateNotExist = validator """{"Ages": {"$all": 45}}"""
        let validateOp = validator """{"Ages": {"$all": { "$gt": 10 }}}"""
        let validateOpNotExist = validator """{"Ages": {"$all": { "$lt": 10 }}}"""
        validate (context [("Ages", ages);])  |> should equal true
        validateNotExist (context [("Ages", ages);])  |> should equal false
        validateOp (context [("Ages", ages);])  |> should equal true
        validateOpNotExist (context [("Ages", ages);])  |> should equal false
    
    [<Fact>]
    member test.``Array comparers - invalid comparer format``() =
        (fun () -> validator """{"Birthdays": {"$any": }}""" |> ignore) |> should throw typeof<Exception>
        (fun () -> validator """{"Birthdays": {"$any": null}}""" |> ignore) |> should throw typeof<Exception>
        (fun () -> validator """{"Birthdays": {"$any": { "Date": { "$operatorNotExist":"value" } } }}""" |> ignore) |> should throw typeof<Exception>

        (fun () -> validator """{"Birthdays": {"$all": }}""" |> ignore) |> should throw typeof<Exception>
        (fun () -> validator """{"Birthdays": {"$all": null}}""" |> ignore) |> should throw typeof<Exception>
        (fun () -> validator """{"Birthdays": {"$all": { "Date": { "$operatorNotExist":"value" } } }}""" |> ignore) |> should throw typeof<Exception>
