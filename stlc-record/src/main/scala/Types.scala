import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree
import syntax.RecordsParserSyntax.{ASTNode, Program}

class Types(tree : Tree[ASTNode, Program]) extends Attribution {

    import syntax.RecordsParserSyntax._
    import org.bitbucket.inkytonik.kiama.util.{Entity, Environments}
    import org.bitbucket.inkytonik.kiama.util.Messaging.{collectMessages, info, Messages}
    import syntax.RecordsParserPrettyPrinter.show

    def ppe(e : Exp) : String = {
        val s = show(e)
        if (s.length < 10)
            s
        else
            s"${s.substring(0, 10)} ..."
    }

    def ppt(ot : Option[Typ]) : String =
        ot match {
            case Some(t) =>
                show(t)
            case _ =>
                "unknown"
        }

    sealed abstract class RecordsEntity extends Entity {
        def show : String
    }

    case class ArgumentEntity(decl : Fun) extends RecordsEntity {
        def show = s"argument ${ppt(entitytype(this))}"
    }

    case class FieldEntity(decl : Field) extends RecordsEntity {
        def show = s"field in value ${ppt(entitytype(this))}"
    }

    case class FieldTypEntity(decl : FieldTyp) extends RecordsEntity {
        def show = s"field in type ${ppt(entitytype(this))}"
    }

    case class VariableEntity(decl : Let) extends RecordsEntity {
        def show = s"variable ${ppt(entitytype(this))}"
    }

    case class MultipleEntity() extends RecordsEntity {
        override val isError = true
        def show = "multiple"
    }

    case class UnknownEntity() extends RecordsEntity {
        override val isError = true
        def show = "unknown"
    }

    object SymbolTable extends Environments[RecordsEntity]
    import SymbolTable._

    lazy val messages : Messages =
        collectMessages(tree) {
            case d @ IdnDef(i) =>
                info(d, s"$i def: ${defentity(d).show}")
            case e : Exp =>
                info(e, s"${ppe(e)}: ${ppt(tipe(e))}")
        }

    val oktype : Typ => Boolean =
        attr {
            case FunT(t, u) =>
                oktype(t) && oktype(u)
            case NumT() =>
                true
            case RecT(fs) =>
                fs.map(_.idnDef.identifier).toSet.size == fs.length
        }

    val defentity : IdnDef => RecordsEntity =
        attr {
            case tree.parent(p : Fun) =>
                ArgumentEntity(p)
            case tree.parent(p : Field) =>
                FieldEntity(p)
            case tree.parent(p : FieldTyp) =>
                FieldTypEntity(p)
            case tree.parent(p : Let) =>
                VariableEntity(p)
            case _ =>
                UnknownEntity()
        }

    val entitytype : RecordsEntity => Option[Typ] =
        attr {
            case ArgumentEntity(decl) if oktype(decl.typ) =>
                Some(decl.typ)
            case FieldEntity(decl) =>
                tipe(decl.exp)
            case FieldTypEntity(decl) if oktype(decl.typ) =>
                Some(decl.typ)
            case VariableEntity(decl) =>
                tipe(decl.exp1)
            case _ =>
                None
        }

    val env : ASTNode => Environment =
        attr {
            case n @ tree.parent(p @ Let(d @ IdnDef(i), _, body)) if n eq body =>
                define(enter(env(p)), i, defentity(d))

            case tree.prev.pair(Field(d @ IdnDef(i), _), p) =>
                defineIfNew(env(p), i, MultipleEntity(), defentity(d))

            case tree.parent.pair(Field(d @ IdnDef(i), _), p) =>
                defineIfNew(enter(env(p)), i, MultipleEntity(), defentity(d))

            case n @ tree.prev.pair(FieldTyp(d @ IdnDef(i), _), p) =>
                defineIfNew(env(p), i, MultipleEntity(), defentity(d))

            case n @ tree.parent.pair(FieldTyp(d @ IdnDef(i), _), p) =>
                defineIfNew(enter(env(p)), i, MultipleEntity(), defentity(d))

            case tree.parent(p @ Fun(d @ IdnDef(i), _, _)) =>
                define(enter(env(p)), i, defentity(d))

            case n @ tree.parent(p @ Wth(e1, e2)) if n eq e2 =>
                tipe(e1) match {
                    case Some(RecT(fs)) =>
                        fs.foldLeft(enter(env(p))) {
                            case (e, FieldTyp(d @ IdnDef(i), _)) =>
                                define(e, i, defentity(d))
                        }
                    case _ =>
                        env(p)
                }

            case tree.parent(p) =>
                env(p)

            case _ =>
                rootenv()
        }

    val entity : IdnUse => RecordsEntity =
        attr {
            case u @ IdnUse(i) =>
                lookup(env(u), i, UnknownEntity())
        }

    def isSubType(t1 : Typ, t2 : Typ) : Boolean =
        (t1, t2) match {
            case (NumT(), NumT()) =>
                true
            case (FunT(t1, u1), FunT(t2, u2)) =>
                isSubType(t2, t1) && isSubType(u1, u2)
            case (RecT(fs1), RecT(fs2)) =>
                isSubTypeFields(fs1, fs2)
            case _ =>
                false
        }

    def isSubTypeFields(fs1 : Vector[FieldTyp], fs2 : Vector[FieldTyp]) : Boolean = {

        def lookup(i : IdnDef) : Option[FieldTyp] =
            fs1.find(_.idnDef == i)

        def aux(fs : Vector[FieldTyp]) : Boolean =
            fs match {
                case Vector() =>
                    true
                case FieldTyp(i, t1) +: rest =>
                    lookup(i) match {
                        case Some(FieldTyp(_, t2)) =>
                            isSubType(t2, t1) && aux(rest)
                        case None =>
                            false
                    }
            }

        aux(fs2)
    }

    val tipe : Exp => Option[Typ] =
        attr {
            // STLC-App
            case App(l, r) =>
                (tipe(l), tipe(r)) match {
                    case (Some(FunT(t1, t2)), Some(t3)) if isSubType(t3, t1) =>
                        Some(t2)
                    case _ =>
                        None
                }

            // STLC-Fun
            case Fun(x, t, e) =>
                if (oktype(t))
                    tipe(e) map (u => FunT(t, u))
                else 
                    None

            // STLC-Id
            case u : IdnUse =>
                entitytype(entity(u))

            // STLC-Let
            case Let(_, _, e) =>
                tipe(e)

            // STLC-Num
            case _ : Num =>
                Some(NumT())

            // STLC-Plus
            case Plus(l, r) =>
                (tipe(l), tipe(r)) match {
                    case (Some(NumT()), Some(NumT())) =>
                        Some(NumT())
                    case _ =>
                        None
                }

            // Extension: Records

            // STLC-Acc
            case Acc(e, i) =>
                tipe(e) match {
                    case Some(RecT(fs)) =>
                        fs.collectFirst {
                            case FieldTyp(IdnDef(j), t) if i == j =>
                                t
                        }
                    case _ =>
                        None
                }

            // STLC-Ext
            case Ext(e1, e2) =>
                (tipe(e1), tipe(e2)) match {
                    case (Some(RecT(fs1)), Some(RecT(fs2))) =>
                        val drop = fs1.map(_.idnDef)
                        val keep = fs2.filter(f => !(drop contains f.idnDef))
                        Some(RecT(fs1 ++ keep))
                    case _ =>
                        None
                }

            // STLC-Rec
            case Rec(Fields(fs)) =>
                val ts = fs map (f => tipe(f.exp))
                if (ts contains None)
                    None
                else
                    Some(RecT(
                        (fs zip ts) map {
                            case (x, t) =>
                                FieldTyp(x.idnDef, t.get)
                        }
                    ))

            // STLC-With
            case Wth(r, e) =>
                tipe(r) match {
                    case Some(_ : RecT) =>
                        tipe(e)
                    case _ =>
                        None
                }

        }

}
