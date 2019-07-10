import org.bitbucket.inkytonik.kiama.util.{CompilerBase, Config}
import syntax.RecordsParserSyntax.{ASTNode, Program}

class Driver extends CompilerBase[ASTNode, Program, Config] {

    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
    import org.bitbucket.inkytonik.kiama.util.Source
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import syntax.RecordsParser
    import syntax.RecordsParserPrettyPrinter
    // import syntax.RecordsParserPrettyPrinter.{any, layout}

    val name = "records"

    def createConfig(args : Seq[String]) : Config =
        new Config(args)

    override def makeast(source : Source, config : Config) : Either[Program,Messages] = {
        val p = new RecordsParser(source, positions)
        val pr = p.pProgram(0)
        if (pr.hasValue)
            Left(p.value(pr).asInstanceOf[Program])
        else
            Right(Vector(p.errorToMessage (pr.parseError)))
    }

    def process (source : Source, p : Program, config : Config) {
        import org.bitbucket.inkytonik.kiama.relation.Tree

        val tree = new Tree[ASTNode, Program](p)
        val types = new Types(tree)
        import types.{tipe, ppt}

        // config.output().emitln(format(p).layout)
        // config.output().emitln(layout(any(p)))
        // config.output().emitln()

        report(source, types.messages, config)

        config.output().emitln(ppt(tipe(p.exp)))
    }

    override def format(ast : Program) : Document =
        RecordsParserPrettyPrinter.format(ast, 5)

}

object Main extends Driver
