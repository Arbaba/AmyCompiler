package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v
    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
			//println(s"expected $expected for $e")
			// This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
				List(Constraint(found, expected, e.position))



			def genBinOp(lhs: Expr, rhs: Expr, opTpe: Type, retTpe: Type): List[Constraint] = {
				topLevelConstraint(retTpe) ++ genConstraints(lhs, opTpe) ++ genConstraints(rhs, opTpe)
			}
      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)

				case BooleanLiteral(_) => topLevelConstraint(BooleanType)
				case StringLiteral(_) => topLevelConstraint(StringType)
				case UnitLiteral() => topLevelConstraint(UnitType)

				case Variable(n) => topLevelConstraint(env(n))

        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
					genBinOp(lhs, rhs, TypeVariable.fresh, BooleanType)
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) = pat match {
							case WildcardPattern() => (Nil, Map())
							case IdPattern(litty) => (Nil, Map(litty -> TypeVariable.fresh))
							case LiteralPattern(litty) => (genConstraints(litty, scrutExpected), Map())
							case CaseClassPattern(qname, args) =>
								val constructConstr = Constraint(ClassType(qname), scrutExpected, e.position)
								//val (cstr, en) = args.flatMap(arg => handlePattern(arg, env(arg.))).unzip
								println(s"qname: $qname types:: ${table.types}")
								println(s"Constructor all ${(table.constructors)}")
								println(s"Constructor manual ${(table.constructors.get(qname))}")
								println(s"Constructor ${(table getConstructor qname)}")
								println(s"Constructors ${(table getConstructorsForType qname)}")
								println(s"Constructor unqualified ${(table.getConstructor("L", qname.name))}")
								
								val argTypes: List[Type] = (table getConstructor qname).get.argTypes
								val css: List[(List[Constraint], Map[Identifier, Type])] = (args zip argTypes).map{ case (p, t) => handlePattern(p, t) }
								type Pat = (List[Constraint], Map[Identifier, Type])
								val (constraints, envs) = css.fold[Pat](List(), Map()){ case (c1: Pat, c2: Pat) => (c1._1 ++ c2._1, c2._2 ++ c1._2)}
								(List(constructConstr) ++ constraints, envs)
						}

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        case Plus(lhs, rhs) => genBinOp(lhs, rhs, IntType, IntType)
				case Minus(lhs, rhs) => genBinOp(lhs, rhs, IntType, IntType)
				case Times(lhs, rhs) => genBinOp(lhs, rhs, IntType, IntType)
				case Div(lhs, rhs) => genBinOp(lhs, rhs, IntType, IntType)
				case Mod(lhs, rhs) => genBinOp(lhs, rhs, IntType, IntType)
				case LessThan(lhs, rhs) => genBinOp(lhs, rhs, IntType, BooleanType)
				case LessEquals(lhs, rhs) => genBinOp(lhs, rhs, IntType, BooleanType)
				case And(lhs, rhs) => genBinOp(lhs, rhs, BooleanType, BooleanType)
				case Or(lhs, rhs) => genBinOp(lhs, rhs, BooleanType, BooleanType)
				case Concat(lhs, rhs) =>
					genBinOp(lhs, rhs, StringType, StringType)
				case Not(expr) =>
					topLevelConstraint(BooleanType) ++ genConstraints(expr, BooleanType)
				case Neg(expr) =>
					topLevelConstraint(IntType) ++ genConstraints(expr, IntType)
				case Call(qname, args) =>
					def getFunOrBuilder(): (List[Type], Type) = (table getConstructor qname, table getFunction qname) match {
						case (Some(ConstrSig(argTypes, _, _)), _) =>  (argTypes, ClassType(qname))
						case (_, Some(FunSig(argTypes, retType, _))) => (argTypes, retType)
					}
					val (in, out) = getFunOrBuilder()
					val argConstraints = (args zip in) flatMap { case (expr, tpe) => genConstraints(expr, tpe) }
					topLevelConstraint(out) ++ argConstraints
				case Let(df, va, bdy) =>
					val tpe = df.tt.tpe
					val v = genConstraints(va, tpe)
					genConstraints(bdy, expected)(env + (df.name -> tpe)) ++ v
				case Sequence(s1, s2) =>
					genConstraints(s1, TypeVariable.fresh) ++ genConstraints(s2, expected)
				case Ite(i, t, el) =>
					val tA = expected//TypeVariable.fresh
					genConstraints(i, BooleanType) ++ topLevelConstraint(tA) ++ genConstraints(t, tA) ++ genConstraints(el, tA)
				case Error(s) => genConstraints(s, StringType) ++ topLevelConstraint(expected)
				case _ => ???
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
					expected match {
						case TypeVariable(i) =>
							solveConstraints(subst_*(more, i, found))
						case _ =>
							if(found != expected) fatal(s"$found should be $expected", pos)
							else solveConstraints(more)
					}
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
				//println(s"env $env")
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }
    v

  }
}
