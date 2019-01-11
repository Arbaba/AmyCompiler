package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[(N.Program, SymbolTable), (S.Program, SymbolTable)] {
  def run(ctx: Context)(input: (N.Program, SymbolTable)): (S.Program, SymbolTable) = {
    import ctx.reporter._
		val (p, table) = input
def transformType(tt: N.TypeTree, inModule: String): S.Type = {
	tt.tpe match {
		case N.IntType => S.IntType
		case N.BooleanType => S.BooleanType
		case N.StringType => S.StringType
		case N.UnitType => S.UnitType
		case N.ClassType(qn@N.QualifiedName(module, name)) =>
			table.getType(module getOrElse inModule, name) match {
				case Some(symbol) =>
					S.ClassType(symbol)
				case None =>
					fatal(s"Could not find type $qn in $inModule", tt)
			}
	}
}
    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones
		def getTypeID(module: String, name: String): Identifier = table getType (module, name) match {
			case Some(id) => id
			case _ => fatal(s"$name not found in $module")
		}

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case N.AbstractClassDef(name) => S.AbstractClassDef(getTypeID(module, name))
      case N.CaseClassDef(name, fields, parent) =>
				val tts = fields map { case tt: N.TypeTree => S.TypeTree(transformType(tt, module)) }
				S.CaseClassDef(getTypeID(module, name), tts, getTypeID(module, parent))
      case fd: N.FunDef => transformFunDef(fd, module)
			case od: N.OpDef => transformOpDef(od, module)

    }}.setPos(df)

		def transformOpDef(fd: N.OpDef, module: String): S.OpDef = {
      val N.OpDef(name, params, retType, body, precedence) = fd
      val Some((sym, sig)) = table.getOperator(name)
			//operator has 2 arguments, enforced by grammar
      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.OpDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map())),
				precedence
      ).setPos(fd)
    }

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

		def transformLit[T](lit: N.Literal[T]): S.Literal[T] = lit match {
			case N.UnitLiteral() => S.UnitLiteral()
			case N.BooleanLiteral(b) => S.BooleanLiteral(b)
			case N.IntLiteral(i) => S.IntLiteral(i)
			case N.StringLiteral(s) => S.StringLiteral(s)
		}

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
			val (params, locals) = names
			val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match {
							case N.WildcardPattern() =>
								(S.WildcardPattern(), Nil)
							case N.LiteralPattern(lit) => (S.LiteralPattern(transformLit(lit)), Nil)
							case N.IdPattern(name) =>
								val newId = Identifier fresh name
								(S.IdPattern(newId), (name, newId) :: Nil)
							case N.CaseClassPattern(qname, args) =>
								//val name = Identifier fresh qname.name
								val (patterns, bindings) = (args map transformPattern).unzip
								bindings.flatten groupBy { case (a, b) => a } foreach {
									case (s, ids) => if(ids.size > 1) fatal("")
								}
								(S.CaseClassPattern(table getType (module, qname.name) match {
										case Some(tpeCons) => tpeCons
										case None => fatal(s"could not get type ${qname.name} in ${qname.module}")
									}
								, patterns), bindings.flatten)
						}
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
						moreLocals foreach { case (s, id) => if((names._2 contains s) || (names._1 contains s)) fatal(s"$s bad thing $names") }
						S.MatchCase(newPat, transformExpr(rhs)(module, (names._1, names._2 ++ moreLocals.toMap)))
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

				case N.Plus(lhs, rhs) => S.Plus(transformExpr(lhs), transformExpr(rhs))
				case N.Minus(lhs, rhs) => S.Minus(transformExpr(lhs), transformExpr(rhs))
				case N.Times(lhs, rhs) => S.Times(transformExpr(lhs), transformExpr(rhs))
				case N.Div(lhs, rhs) => S.Div(transformExpr(lhs), transformExpr(rhs))
				case N.Mod(lhs, rhs) => S.Mod(transformExpr(lhs), transformExpr(rhs))
				case N.LessThan(lhs, rhs) => S.LessThan(transformExpr(lhs), transformExpr(rhs))
				case N.LessEquals(lhs, rhs) => S.LessEquals(transformExpr(lhs), transformExpr(rhs))
				case N.And(lhs, rhs) => S.And(transformExpr(lhs), transformExpr(rhs))
				case N.Or(lhs, rhs) => S.Or(transformExpr(lhs), transformExpr(rhs))
				case N.Equals(lhs, rhs) => S.Equals(transformExpr(lhs), transformExpr(rhs))
				case N.Concat(lhs, rhs) => S.Concat(transformExpr(lhs), transformExpr(rhs))
				case N.IntLiteral(i) => S.IntLiteral(i)
				case N.StringLiteral(s) => S.StringLiteral(s)
				case N.BooleanLiteral(b) => S.BooleanLiteral(b)
				case N.UnitLiteral() => S.UnitLiteral()
				case N.Variable(name) => S.Variable((names._1 get name, names._2 get name) match
				{ case (Some(n), _) => n; case (_, Some(n)) => n; case (None, None) => fatal(s"$name not found: $expr in $names")})
				case N.Let(pdef, value, bdy) => {
					val defi = pdef match {
						case N.ParamDef(name, tt) =>{
							if((names._1 contains name) || (names._2.contains(name))) fatal(s"local $name redefined")
							val id = Identifier fresh name
							S.ParamDef(id, S.TypeTree(transformType(tt, module)))
						}
					}
					val bind = transformExpr(value)
					S.Let(defi, bind, transformExpr(bdy)(module, (names._1, names._2 + (pdef.name -> defi.name))))
				}
				case N.Not(expr) => S.Not(transformExpr(expr))
				case N.Neg(expr) => S.Neg(transformExpr(expr))
				case N.Call(qname, args) =>
					val mod = qname.module match {
						case Some(actual) => actual
						case None => module
					}
					(table getFunction (mod, qname.name),
					table getConstructor (mod, qname.name),
					table getOperator (qname.name)) match {
						case (_, _, Some(operator)) => //we already know that the number of arguments matches
							S.Call(operator._1, args map transformExpr)
						case (_, Some((id, sig)), _) =>
							if(args.size != sig.argTypes.size) fatal("UndefinedConstructor")
							S.Call(id, args map transformExpr)
						//Regular function
						case (Some(s), _, _) =>
							if(args.size != s._2.argTypes.size) fatal("arguments!")
							S.Call(s._1, args map transformExpr)//(modules, names._1, names._2 ++ ))
						case _ => fatal(s"Did not found $qname")
					}

				case N.Sequence(expr1, expr2) => S.Sequence(transformExpr(expr1), transformExpr(expr2))
				case N.Ite(i, t, e) => S.Ite(transformExpr(i), transformExpr(t), transformExpr(e))
				case N.Error(msg) => S.Error(transformExpr(msg))
        case _ =>
          fatal(s"$expr not implemented")//???  // TODO: Implement the rest of the cases
      }
      res.setPos(expr)
    }
    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
