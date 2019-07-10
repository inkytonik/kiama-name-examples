import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree
import syntax.FWJavaParserSyntax.{ASTNode, Program}


class Types(tree : Tree[ASTNode, Program]) extends Attribution {

    import syntax.FWJavaParserSyntax._
    import org.bitbucket.inkytonik.kiama.util.Messaging.{collectMessages, error, info, Messages, noMessages}
    import syntax.FWJavaParserPrettyPrinter.show
    import tree._

    type Defs = Vector[D]
    
    // Pretty-printing helpers
    
    def ppe(e : E) : String = {
        val s = show(e)
        if (s.length < 10)
            s
        else
            s"${s.substring(0, 10)} ..."
    }

    def ppt(ot : Option[String]) : String =
        ot match {
            case Some(c) =>
                c
            case _ =>
                "unknown"
        }

    def ppn(names : Vector[String]) : String =
        names.mkString(", ")

    def ppd(defs : Defs) : String =
        defs.map(show(_)).mkString(", ")
        
    // Accessors

    def className(l : L) : String =
        l.identifier

    def superClassName(l : L) : String =
        l.idnUse.identifier

    def fieldNames(l : L) : Vector[String] =
        l.optDs.map(_.idnDef.identifier)

    def fieldTypes(l : L) : Vector[String] =
        l.optDs.map(_.idnUse.identifier)
        
    def methodName(m : M) : String =
        m.identifier
        
    def argTypes(m : M) : Vector[String] =
        m.optDs.map(_.idnUse.identifier)
        
    def returnType(m : M) : String =
        m.idnUse.identifier
        
    def constructorName(k : K) : String =
        k.idnUse.identifier      
        
    def constructorParams(k : K) : Defs =
        k.optDs
        
    def superArgs(k : K) : Vector[IdnUse] =
        k.idnUses.optIdnUses
        
    def superArgNames(k : K) : Vector[String] =
        superArgs(k).map(_.identifier)
        
    // Top-level message collection
        
    lazy val messages : Messages =
        collectMessages(tree) {
            case d @ parent.pair(IdnDef(i), parent(p)) =>
                info(d, s"$i def: ${ppt(findType(env(d), i))}")
            case e : E =>
                info(e, s"${ppe(e)}: ${ppt(tipe(e))}")
            case l : L =>
                classOK(l)
        }
        
    // OK checking    
    
    val classOK : L => Messages =
        attr {
            case l =>
                val cycles = superClassCycle(l)(l)
                if (cycles.isEmpty)
                    constructorOK(l) ++ l.optMs.flatMap(methodOK)
                else 
                    cycles
        }
        
    val constructorOK : L => Messages =
        attr {
            case l =>
                superClassDefined(l) ++
                    constructorNameOK(l) ++ constructorParamsOK(l) ++
                    initialisersOK(l) ++ superConstructorCallOK(l)
        }
        
    val superClassDefined : L => Messages =
        attr {
            case l =>
                if ((superClassName(l) == "Object") || decl(superClassName(l))(l).isDefined)
                    noMessages
                else 
                    error(l.idnUse, s"superclass ${superClassName(l)} is not defined")
        }
        
    val superClassCycle : L => L => Messages =
        paramAttr {
            case base => {
                case l =>
                    decl(superClassName(l))(l) match {
                        case Some(l2) =>
                            if (className(l2) == className(base))
                                error(base, s"superclass cycle from ${className(base)}") 
                            else 
                                superClassCycle(base)(l2)
                        case None =>
                            noMessages
                    }
            }
        }
        
    val constructorNameOK : L => Messages =
        attr {
            case l =>
                if (className(l) == constructorName(l.k))  
                    noMessages
                else
                    error(l.k, s"constructor name ${constructorName(l.k)} is not class name ${className(l)}")
        }
        
    val constructorParamsOK : L => Messages =
        attr {
            case l =>
                fields(l.identifier)(l) match {
                    case Some(defs) =>
                        if (constructorParams(l.k) == defs)
                            noMessages
                        else 
                            error(l.k, s"constructor args (${ppd(constructorParams(l.k))}) should be the fields (${ppd(defs)})")
                    case None =>
                        noMessages
                }
        }
        
    val initialisersOK : L => Messages = 
        attr {
            case l =>
                l.k.is.optIs.flatMap(initialiserOK)
        }
        
    val initialiserOK : I => Messages = 
        attr {
            case parent.pair(i : I, parent(parent(l : L))) =>
                val f = fieldNames(l)(index(i))
                if ((i.idnUse1.identifier == f) && (i.idnUse2.identifier == f))
                    noMessages 
                else
                    error(i, s"initialiser must be this.$f = $f")
        }
        
    val superConstructorCallOK : L => Messages = 
        attr {
            case l =>
                decl(superClassName(l))(l) match {
                    case Some(l2) =>
                        superArgs(l.k).map(superConstructorArgNameOK(l2)).flatten ++
                            superArgs(l.k).map(superConstructorArgTypeOK(l2)).flatten
                    case _ =>
                        noMessages
                }
        }
        
    val superConstructorArgNameOK : L => IdnUse => Messages =
        paramAttr {
            case l => {
                case idnUse @ IdnUse(x) =>
                    val n = index(idnUse)
                    val f = fieldNames(l)(n)
                    if (x == f)
                        noMessages 
                    else
                        error(idnUse, s"super arg $n must be $f, not $x")
            }
        }

    val superConstructorArgTypeOK : L => IdnUse => Messages =
        paramAttr {
            case l => {
                case idnUse =>
                    val n = index(idnUse)
                    val t1 = fieldTypes(l)(n)
                    idnUseType(idnUse) match {
                        case Some(t2) if t1 != t2 =>
                            error(idnUse, s"super arg $n type must be $t1, not $t2")
                        case _ =>
                        noMessages 
                    }
            }
        }
        
    val methodOK : M => Messages =
        attr {
            case m =>
                returnTypeOK(m) ++ overrideOK(m)
        }
        
    val returnTypeOK : M => Messages =
        attr {
            case m =>
                if (optsubtype((bodyType(m), returnType(m)))(m))
                    noMessages 
                else
                    error(m, s"body type ${bodyType(m)} is not a subtype of declared return type ${returnType(m)}")
        }

    val bodyType : M => Option[String] =
        attr {
            case m =>
                tipe(m.e)
        }
        
    val overrideOK : M => Messages =
        attr {
            case parent.pair(m : M, l : L) =>
                decl(superClassName(l))(l) match {
                    case Some(l2) =>
                        method(methodName(m))(l2) match {
                            case Some(sm) =>
                                if ((returnType(m) != returnType(sm)) || (argTypes(m) != argTypes(sm)) )
                                    error(m, s"""method type (${ppn(argTypes(m))}) : ${returnType(m)} does not match overridden type (${ppn(argTypes(sm))}) : ${returnType(sm)}""")
                                else
                                    noMessages                             
                            case None =>
                                noMessages
                        }
                    case None =>
                        noMessages
                }
        }
        
    // Names via environments
        
    val env : ASTNode => Defs =
        attr {
            case l @ L(c, _, _, _, _) =>
                fields(c)(l).get :+ D(IdnUse(c), IdnDef("this"))

            case k : K =>
                k.optDs

            case parent.pair(m : M, p) =>
                m.optDs ++ env(p)
            
            case parent(p) =>
                env(p)

            case _ =>
                Vector()
        }
        
    // Types

    def findType(defs : Defs, x : String) : Option[String] =
        defs.collectFirst {
            case D(IdnUse(c), IdnDef(y)) if x == y =>
                c
        }
        
    val idnUseType : IdnUse => Option[String] = 
        attr {
            case u @ IdnUse(x) =>
                findType(env(u), x)
        }

    val tipe : E => Option[String] =
        attr {
            // VAR
            case Idn(u) =>
                idnUseType(u)
                
            // FLD
            case Fld(e, IdnUse(f)) =>
                tipe(e) match {
                    case Some(c) =>
                        fields(c)(e) match {
                            case Some(defs) =>
                                findType(defs, f)
                            case _ =>
                                None
                        }
                    case None =>
                        None
                }
                
            // INV
            case n @ Inv(e, IdnUse(m), es) =>
                tipe(e) match {
                    case Some(c) =>
                        decl(c)(e) match {
                            case Some(l) => 
                                method(m)(l) match {
                                    case Some(M(IdnUse(r), _, defs, _)) =>
                                        if (esubtypes((es, defs))(n))
                                            Some(r)
                                        else
                                            None
                                    case None =>
                                        None
                                }
                            case None =>
                                None
                        }
                    case None =>
                        None
                }
            
            // NEW
            case n @ New(IdnUse(c), es) =>
                fields(c)(n) match {
                    case Some(flds) =>
                        if (esubtypes((es, flds))(n))
                            Some(c)
                        else
                            None
                    case _ =>
                        None
                }
                
            // UCAST, DCAST
            case Cst(IdnUse(c), e) =>
                tipe(e) match {
                    case Some(d) =>
                        Some(c)
                    case None =>
                        None
                }
                
        }
        
    // Lookups
        
    val decl : String => ASTNode => Option[L] =
        paramAttr {
            c => {
                case parent(p) =>
                    decl(c)(p)
                case Program(ls, _) =>
                    ls.find(l => className(l) == c)
            }
        }
        
    val method : String => L => Option[M] =
        paramAttr {
            m => {
                case l1 @ L(_, IdnUse(c), _, _, ms) =>
                    ms.find(_.identifier == m) match {
                        case Some(meth) =>
                            Some(meth)
                        case None =>
                            decl(c)(l1) match {
                                case Some(l2) =>
                                    method(m)(l2)                                
                                case None => 
                                    None
                            }
                    }
            }
        }
        
    val fields : String => ASTNode => Option[Defs] =
        paramAttr {
            case "Object" => {
                case n =>
                    Some(Vector())
            }
            case c => {
                case n =>
                    decl(c)(n) match {
                        case Some(L(_, i @ IdnUse(sc), defs1, _, _)) =>
                            fields(sc)(i) match {
                                case Some(defs2) =>
                                    Some(defs1 ++ defs2)
                                case None =>
                                    None
                            }
                        case None =>
                            None
                    }
            }
        }
        
    // Sub-typing

    val esubtypes : CachedParamAttribute[(Vector[E], Defs), ASTNode, Boolean] =
        paramAttr {
            case (es, defs) => {
                case n =>
                    val ts = es.map(tipe)
                    (ts.length == defs.length) &&
                        (!ts.contains(None)) &&
                        subtypes((ts.map(_.get), defs))(n)
            }
        }

    val subtypes: CachedParamAttribute[(Vector[String], Defs), ASTNode, Boolean] =
        paramAttr {
            case (ts, defs) => {
                case n =>
                    ts.zip(defs).forall {
                        case (t, D(IdnUse(c), _)) =>
                            subtype((t, c))(n)
                    }
            }
        }
        
    val subtype: CachedParamAttribute[(String, String), ASTNode, Boolean] =
        paramAttr {
            case (c1, c2) => {
                case n =>
                    (c1 == c2) || 
                    (decl(c1)(n) match {
                        case Some(L(_, IdnUse(c3), _, _, _)) =>
                            if (c2 == c3)
                                true 
                            else 
                                subtype((c3, c2))(n)
                        case _ =>
                            false
                    })
            }
        }
        
    val optsubtype: CachedParamAttribute[(Option[String], String), ASTNode, Boolean] =
        paramAttr {
            case (Some(c1), c2) => {
                case n =>
                    subtype((c1, c2))(n)
            }
            case (None, _) => {
                case n =>
                    true
            }
        }
        
}
