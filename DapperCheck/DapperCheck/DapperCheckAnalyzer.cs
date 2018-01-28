using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace DapperCheck
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class DapperCheckAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "DapperCheck";

        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Naming";

        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Error, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeDapperCall, SyntaxKind.InvocationExpression);
        }

        private static string NormaliseColumnName(string givenColumnClause)
        {
            var toReturn = givenColumnClause;
            if (toReturn.Contains('.'))
            {
                toReturn = toReturn.Split('.')[1];
            }

            if (toReturn.Contains("AS"))
            {
                toReturn = toReturn.Split(new string[] { "AS" }, StringSplitOptions.RemoveEmptyEntries)[1];
            }

            // TODO: Ewwww
            if (toReturn.Contains("as"))
            {
                toReturn = toReturn.Split(new string[] { "as" }, StringSplitOptions.RemoveEmptyEntries)[1];
            }

            return toReturn.Trim();
        }


        // TODO: Use a proper SQL parser
        private static List<string> GetColumnNamesFromSql(string givenSql)
        {
            var selectClause = givenSql.Split(new string[] { "FROM" }, StringSplitOptions.RemoveEmptyEntries).First().Substring(7);
            return selectClause.Split(',').Select(s => s.Trim()).Select(NormaliseColumnName).ToList();
        }

        private static void AnalyzeDapperCall(SyntaxNodeAnalysisContext context)
        {
            var invocationExpression = (InvocationExpressionSyntax) context.Node;
            var memberAccessExpr = invocationExpression.Expression as MemberAccessExpressionSyntax;
            if (!memberAccessExpr?.Name.ToString().StartsWith("Query<") ?? true) return;

            // Check we called the Typed query expression method
            var memberSymbol = context.SemanticModel.GetSymbolInfo(memberAccessExpr).Symbol as IMethodSymbol;
            if (memberSymbol?.ToString().StartsWith("System.Data.IDbConnection.Query<T>") ?? true) return;

            // Check we have at least one argument
            // TODO: Needed?
            var argumentList = invocationExpression.ArgumentList as ArgumentListSyntax;
            if ((argumentList?.Arguments.Count ?? 0) < 1) return;

            // Get the compile-time value of the sql argument
            var sqlLiteral = argumentList?.Arguments[0].Expression as IdentifierNameSyntax;
            if (sqlLiteral == null) return;
            var sqlOpt = context.SemanticModel.GetConstantValue(sqlLiteral);
            if (!sqlOpt.HasValue) return; // Couldn't resolve the compile-time value
            var sql = sqlOpt.Value as string;
            if (string.IsNullOrEmpty(sql)) return;

            // Get the type on the query generic
            var deserializationTargetTypeSymbols = memberSymbol.TypeArguments;
            if (deserializationTargetTypeSymbols == null) return; // This was not a typed call?
            if (deserializationTargetTypeSymbols.Length != 1) return; // TODO: Support nested types??
            var deserializationTargetTypeSymbol = deserializationTargetTypeSymbols.First() as INamedTypeSymbol;
            if (deserializationTargetTypeSymbol == null) return; // TODO: Needed?

            // Get the public parameters
            // TODO: Need to check base class?
            var deserializationTargetMemberNames = deserializationTargetTypeSymbol.MemberNames;

            // Get the column names from SQL
            var columns = GetColumnNamesFromSql(sql);

            // Compare the two collections
            var missingColumns = columns.Where(c => !deserializationTargetMemberNames.Contains(c));
            foreach (var column in missingColumns)
            {
                // Raise alerts for missing column names
                var diagnostic = Diagnostic.Create(Rule, sqlLiteral.GetLocation(), column, deserializationTargetTypeSymbol.Name);
                context.ReportDiagnostic(diagnostic);
            }
        }
    }
}
