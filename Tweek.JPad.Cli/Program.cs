using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using FSharpUtils.Newtonsoft;
using LanguageExt;
using Microsoft.Extensions.CommandLineUtils;
using static LanguageExt.FSharp;
using Microsoft.FSharp.Core;
using Newtonsoft.Json.Linq;

namespace Tweek.JPad.Cli
{
    class Program
    {
        private static readonly JPadParser JPadParser = CreateParser();

        static int Main(string[] args)
        {
            var app = new CommandLineApplication();
            app.Name = "jpad-cli";
            app.HelpOption("-?|-h|--help");

            app.Command("compile", Compile);

            app.Command("calc", Calculate);

            return app.Execute(args);
        }

        private static JPadParser CreateParser()
        {
            var sha1Provider = (Sha1Provider) (s =>
            {
                using (var sha1 = System.Security.Cryptography.SHA1.Create())
                {
                    return sha1.ComputeHash(s);
                }
            });

            var comparers = new Dictionary<string, ComparerDelegate>
            {
                ["version"] = Version.Parse
            };

            var parserSettings = new ParserSettings(sha1Provider,
                FSharpOption<IDictionary<string, ComparerDelegate>>.Some(comparers));
            return new JPadParser(parserSettings);
        }

        private static void Compile(CommandLineApplication command)
        {
            command.Description = "compile rules to check if jpad is valid";
            command.HelpOption("-?|-h|--help");

            var fileArgument = command.Argument("[jpad-file]", "Path to jpad file");

            command.OnExecute(() =>
            {
                var compiled = Compile(fileArgument);
                if (compiled == null) return 1;
                Console.WriteLine("OK");
                return 0;
            });
        }

        private static void Calculate(CommandLineApplication command)
        {
            command.Description = "calculates rule value";
            command.HelpOption("-?|-h|--help");

            var fileArgument = command.Argument("[jpad-file]", "Path to jpad file");

            var contextOptions = command.Option("-c|--context <matcherProperty=value>", "Context for calculation",
                CommandOptionType.MultipleValue);

            command.OnExecute(() =>
            {
                var compiled = Compile(fileArgument);
                if (compiled == null) return 1;

                var contextRegex = new Regex(@"(.+?)=(.+)", RegexOptions.Compiled);

                IReadOnlyDictionary<string, JsonValue> context = contextOptions.Values
                    .Select(x => contextRegex.Match(x))
                    .Select(match => new { key = match.Groups[1].Value, value = match.Groups[2].Value })
                    .ToDictionary(x => x.key, x => JsonValue.NewString(x.value), StringComparer.OrdinalIgnoreCase);

                var result = fs(compiled.Invoke(s => fs(context.TryGetValue(s))));

                Console.WriteLine(result.IfNone(() => JsonValue.Null));

                return 0;
            });
        }

        private static JPadEvaluateExt Compile(CommandArgument fileArgument)
        {
            var rule = File.ReadAllText(fileArgument.Value);
            try
            {
                return JPadParser.Parse.Invoke(rule);
            }
            catch
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine("jpad is not valid");
                return null;
            }
        }
    }
}