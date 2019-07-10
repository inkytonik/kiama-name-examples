import org.bitbucket.inkytonik.kiama.util.TestCompiler
import syntax.RecordsParserSyntax.{ASTNode, Program}

class Tests extends Driver with TestCompiler[ASTNode, Program] {
    filetests("stlc", "src/test/resources/stlc", ".stlc", ".expected")
    filetests("records", "src/test/resources/records", ".stlc", ".expected")
}
