using System;
using System.Linq;
using System.Text.RegularExpressions;
using Wasm;
using Wasm.Interpret;
namespace dotwasm
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length == 0 || args[0] == "--help" || args[0] == "-h")
            {
                Console.WriteLine("Usage: dotnet dotwasm.dll <PATH-TO-WASM>");
                Console.WriteLine("");
                Console.WriteLine("Ather the '>' of the shell. Call your function using normal c/c++/c# call style");
                Console.WriteLine("");
                Console.WriteLine("> main(1,1.0,1.0f)");
            }

            Console.WriteLine($"Loading: {args[0]}");

            var importer = new PredefinedImporter();
            var wasmFile = WasmFile.ReadBinary(args[0]);
            ModuleInstance module = ModuleInstance.Instantiate(wasmFile, importer);

            Console.WriteLine($"Done.");

            while (true)
            {
                Console.Write("> ");
                var line = Console.ReadLine();
                var match = Regex.Match(line, @"(?<FUNCNAME>[a-zA-Z]*)\((?<ARGS>([0-9\.]+f?,?)*)\);?");
                if (!match.Success)
                {
                    Console.Write("Syntax Error!");
                    continue;
                }
                var f = match.Groups["FUNCNAME"].Value;
                var fargs = match.Groups["ARGS"].Value.Split(',', StringSplitOptions.RemoveEmptyEntries)
                    .Select(x =>
                    {
                        if (x.Contains(".") && x.Contains("f")) return float.Parse(x);
                        else if (x.Contains(".") && !x.Contains("f")) return double.Parse(x);
                        else return int.Parse(x);
                    }).Cast<object>().ToList().AsReadOnly();

                try
                {
                    var result = module.ExportedFunctions[f].Invoke(fargs);

                    foreach (var r in result)
                    {
                        Console.WriteLine(r.ToString());
                    }
                }
                catch (WasmException e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }
    }
}
