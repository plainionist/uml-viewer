using System.Text.Json;
using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.MSBuild;

internal static class Program
{
    private static readonly HashSet<string> ExcludedDirectories =
        new(StringComparer.OrdinalIgnoreCase) { "bin", "obj", ".git", ".uml-viewer", "node_modules" };

    private static async Task<int> Main(string[] args)
    {
        try
        {
            if (args.Length != 1)
                throw new ArgumentException("Usage: UmlViewer.CSharp <source-directory|project.csproj|solution.sln>");

            var root = Path.GetFullPath(args[0]);
            var graph = new Graph();
            if (Directory.Exists(root))
            {
                graph.AddCompilation(await LoadDirectory(root));
            }
            else if (File.Exists(root) && Path.GetExtension(root) is ".csproj" or ".sln")
            {
                MSBuildLocator.RegisterDefaults();
                await LoadWorkspace(root, graph);
            }
            else
            {
                throw new ArgumentException($"Not a source directory, .csproj, or .sln: {root}");
            }

            Console.WriteLine(JsonSerializer.Serialize(graph.Export()));
            return 0;
        }
        catch (Exception error)
        {
            Console.Error.WriteLine($"C# scan failed: {error.Message}");
            return 1;
        }
    }

    private static IEnumerable<string> SourceFiles(string directory)
    {
        foreach (var file in Directory.EnumerateFiles(directory, "*.cs").Order(StringComparer.Ordinal))
            yield return file;
        foreach (var child in Directory.EnumerateDirectories(directory).Order(StringComparer.Ordinal))
        {
            if (ExcludedDirectories.Contains(Path.GetFileName(child)) ||
                File.GetAttributes(child).HasFlag(FileAttributes.ReparsePoint))
                continue;
            foreach (var file in SourceFiles(child))
                yield return file;
        }
    }

    private static async Task<Compilation> LoadDirectory(string root)
    {
        var trees = new List<SyntaxTree>();
        foreach (var file in SourceFiles(root))
            trees.Add(CSharpSyntaxTree.ParseText(await File.ReadAllTextAsync(file),
                new CSharpParseOptions(LanguageVersion.Preview), file));
        var references = ((string?)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") ?? "")
            .Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
            .Select(path => MetadataReference.CreateFromFile(path));
        return CSharpCompilation.Create("UmlViewer.Scan", trees, references,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));
    }

    private static async Task LoadWorkspace(string root, Graph graph)
    {
        using var workspace = MSBuildWorkspace.Create();
        workspace.WorkspaceFailed += (_, failure) => Console.Error.WriteLine(failure.Diagnostic.Message);
        var projects = Path.GetExtension(root) == ".sln"
            ? (await workspace.OpenSolutionAsync(root)).Projects
            : new[] { await workspace.OpenProjectAsync(root) };
        foreach (var project in projects.Where(project => project.Language == LanguageNames.CSharp)
                     .OrderBy(project => project.FilePath, StringComparer.Ordinal))
        {
            var compilation = await project.GetCompilationAsync();
            if (compilation is null)
                throw new InvalidOperationException($"Cannot load compilation for {project.FilePath}");
            graph.AddCompilation(compilation);
        }
    }

    private sealed class Graph
    {
        private readonly Dictionary<string, Dictionary<string, object?>> classes = new(StringComparer.Ordinal);
        private readonly Dictionary<(string From, string To), string> edges = new();

        private static bool IsDeclaration(SyntaxNode node) =>
            node is BaseTypeDeclarationSyntax or DelegateDeclarationSyntax;

        private static string TypeId(INamedTypeSymbol type)
        {
            type = type.OriginalDefinition;
            var prefix = type.ContainingType is not null
                ? TypeId(type.ContainingType)
                : type.ContainingNamespace.IsGlobalNamespace ? "" : type.ContainingNamespace.ToDisplayString();
            var name = type.MetadataName.Replace('`', '$');
            return string.IsNullOrEmpty(prefix) ? name : $"{prefix}.{name}";
        }

        private static string TypeName(INamedTypeSymbol type) =>
            type.Name + (type.TypeParameters.Length == 0
                ? "" : $"<{string.Join(", ", type.TypeParameters.Select(parameter => parameter.Name))}>");

        private static string? Stereotype(INamedTypeSymbol type) => type.TypeKind switch
        {
            TypeKind.Interface => "interface",
            TypeKind.Enum => "enum",
            TypeKind.Struct => "struct",
            TypeKind.Delegate => "delegate",
            _ when type.IsAbstract => "abstract",
            _ when type.IsRecord => "record",
            _ => null
        };

        private static Dictionary<string, object?> Member(ISymbol symbol)
        {
            var access = symbol.DeclaredAccessibility switch
            {
                Accessibility.Private => "-",
                Accessibility.Protected or Accessibility.ProtectedOrInternal or Accessibility.ProtectedAndInternal => "#",
                Accessibility.Internal => "~",
                _ => "+"
            };
            var text = symbol switch
            {
                IMethodSymbol method => $"{(method.MethodKind == MethodKind.Constructor ? method.ContainingType.Name : method.Name)}" +
                    (method.TypeParameters.Length == 0 ? "" : $"<{string.Join(", ", method.TypeParameters.Select(parameter => parameter.Name))}>") +
                    $"({string.Join(", ", method.Parameters.Select(parameter => $"{parameter.RefKind.ToString().ToLowerInvariant().Replace("none", "").Trim()} {parameter.Name} : {parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}".Trim()))})" +
                    (method.MethodKind == MethodKind.Constructor ? "" : $" : {method.ReturnType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}"),
                IPropertySymbol property => $"{property.Name} : {property.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}",
                IFieldSymbol field => $"{field.Name} : {field.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}",
                IEventSymbol eventSymbol => $"{eventSymbol.Name} : {eventSymbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)}",
                _ => symbol.Name
            };
            return new() { ["name"] = symbol.Name, ["text"] = $"{access} {text}",
                ["private"] = symbol.DeclaredAccessibility == Accessibility.Private };
        }

        private void AddClass(INamedTypeSymbol type, bool foreign)
        {
            type = type.OriginalDefinition;
            var id = TypeId(type);
            if (classes.TryGetValue(id, out var existing) && (!foreign || !(bool)existing["foreign"]!))
            {
                if (foreign)
                    return;
                if (!(bool)existing["foreign"]!)
                    throw new InvalidOperationException($"Multiple projects declare {id}; scan these projects separately.");
            }
            var members = type.GetMembers().Where(member => !member.IsImplicitlyDeclared).ToArray();
            classes[id] = new()
            {
                ["id"] = id, ["ns"] = id, ["name"] = TypeName(type), ["lang"] = "csharp",
                ["foreign"] = foreign, ["stereotype"] = Stereotype(type),
                ["file"] = foreign ? null : type.DeclaringSyntaxReferences.Select(reference => reference.SyntaxTree.FilePath)
                    .Order(StringComparer.Ordinal).FirstOrDefault(),
                ["fields"] = foreign ? Array.Empty<object>() : members
                    .Where(member => member is IFieldSymbol or IPropertySymbol or IEventSymbol).Select(Member).ToArray(),
                ["ops"] = foreign ? Array.Empty<object>() : members
                    .OfType<IMethodSymbol>().Where(method => method.MethodKind is MethodKind.Ordinary or MethodKind.Constructor)
                    .Select(Member).ToArray()
            };
        }

        private void Reference(string from, ITypeSymbol? type, string kind = "dependency")
        {
            if (type is IArrayTypeSymbol array)
                Reference(from, array.ElementType);
            else if (type is IPointerTypeSymbol pointer)
                Reference(from, pointer.PointedAtType);
            else if (type is INamedTypeSymbol named && named.TypeKind != TypeKind.Error)
            {
                foreach (var argument in named.TypeArguments)
                    Reference(from, argument);
                if (named.SpecialType != SpecialType.None)
                    return;
                var to = TypeId(named);
                if (to == from)
                    return;
                if (!classes.ContainsKey(to))
                    AddClass(named, true);
                var pair = (from, to);
                if (!edges.ContainsKey(pair) || kind != "dependency")
                    edges[pair] = kind;
            }
        }

        private void ReferenceSymbol(string from, ISymbol? symbol)
        {
            if (symbol is INamedTypeSymbol named)
                Reference(from, named);
            else if (symbol is not null)
            {
                Reference(from, symbol.ContainingType);
                Reference(from, symbol switch
                {
                    ILocalSymbol local => local.Type,
                    IParameterSymbol parameter => parameter.Type,
                    IFieldSymbol field => field.Type,
                    IPropertySymbol property => property.Type,
                    IEventSymbol eventSymbol => eventSymbol.Type,
                    IMethodSymbol method => method.ReturnType,
                    _ => null
                });
            }
        }

        public void AddCompilation(Compilation compilation)
        {
            var declarations = new List<(INamedTypeSymbol Type, SyntaxNode Node, SemanticModel Model)>();
            foreach (var tree in compilation.SyntaxTrees.OrderBy(tree => tree.FilePath, StringComparer.Ordinal))
            {
                if (string.IsNullOrEmpty(tree.FilePath) || tree.FilePath.Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
                        .Any(segment => ExcludedDirectories.Contains(segment)))
                    continue;
                var model = compilation.GetSemanticModel(tree);
                foreach (var node in tree.GetRoot().DescendantNodes().Where(IsDeclaration))
                    if (model.GetDeclaredSymbol(node) is INamedTypeSymbol type)
                        declarations.Add((type, node, model));
            }
            var declared = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
            foreach (var (type, _, _) in declarations)
                if (declared.Add(type))
                    AddClass(type, false);
            foreach (var (type, node, model) in declarations)
            {
                var from = TypeId(type);
                Reference(from, type.BaseType, "inheritance");
                foreach (var contract in type.Interfaces)
                    Reference(from, contract, type.TypeKind == TypeKind.Interface ? "inheritance" : "implements");
                foreach (var name in node.DescendantNodes(child => child == node || !IsDeclaration(child)).OfType<SimpleNameSyntax>())
                    ReferenceSymbol(from, model.GetSymbolInfo(name).Symbol);
            }
        }

        public object Export() => new
        {
            classes = classes.OrderBy(pair => pair.Key, StringComparer.Ordinal).Select(pair => pair.Value).ToArray(),
            edges = edges.OrderBy(pair => pair.Key.From, StringComparer.Ordinal).ThenBy(pair => pair.Key.To, StringComparer.Ordinal)
                .Select(pair => new { from = pair.Key.From, to = pair.Key.To, kind = pair.Value }).ToArray()
        };
    }
}