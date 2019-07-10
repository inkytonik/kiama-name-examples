import org.bitbucket.inkytonik.kiama.util.TestCompiler
import syntax.FWJavaParserSyntax.{ASTNode, Program}

class Tests extends Driver with TestCompiler[ASTNode, Program] {
    filetests("scope-paper", "src/test/resources/scope-paper", ".fj", ".expected")
    filetests("tests", "src/test/resources/tests", ".fj", ".expected")
}
