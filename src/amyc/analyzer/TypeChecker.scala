package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

import scala.collection.immutable.StringLike

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
      //println(e)
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      e match {
        //Literals
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)
        case StringLiteral(_)=>
          topLevelConstraint(StringType)
        //Variable
        case Variable(name) =>
          //env.foreach(x => println(x._1.fullName))
          //println(name.fullName)
          topLevelConstraint(env(name))
        //Arith Operators
        case  Plus(lhs: Expr, rhs: Expr)   =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case  Minus(lhs: Expr, rhs: Expr) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case  Times(lhs: Expr, rhs: Expr) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case  Div(lhs: Expr, rhs: Expr) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case  Mod(lhs: Expr, rhs: Expr) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)


        //Boolean Operators
        case  LessThan(lhs: Expr, rhs: Expr)  =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case  LessEquals(lhs: Expr, rhs: Expr)  =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)

        case  And(lhs: Expr, rhs: Expr) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        case  Or(lhs: Expr, rhs: Expr) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        case  Equals(lhs: Expr, rhs: Expr) =>
          val typee = TypeVariable.fresh()
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, typee) ++ genConstraints(rhs, typee)

        //String concatenation
        case  Concat(lhs: Expr, rhs: Expr) =>
          topLevelConstraint(StringType) ++ genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType)

        //Unary Operators
        case Not(e) =>
          topLevelConstraint(BooleanType) ++ genConstraints(e, BooleanType)
        case Neg(e) =>
          topLevelConstraint(IntType) ++ genConstraints(e, IntType)

        //Rest
        case Call(qname, args) =>
           table.getFunction(qname) match {
            case Some(FunSig(argTypes, retType, owner)) =>
              topLevelConstraint(retType) ++ args.flatMap(arg => genConstraints(arg, TypeVariable.fresh()))
            case None =>
              table.getConstructor(qname) match {
                case Some(constrSig) =>
                  topLevelConstraint(constrSig.retType) ++ args.flatMap(arg => genConstraints(arg, TypeVariable.fresh()))
                case None =>
                  table.getOperator(qname) match {
										case Some(opsig) => topLevelConstraint(opsig.retType) ++ args.flatMap(arg => genConstraints(arg, TypeVariable.fresh()))
										case None => fatal(s"Function  ${qname} is not in the sympol table", e.position)
									}
              }
          }
         // topLevelConstraint(env(qname))
        case Sequence(e1, e2) =>
          val t2 = TypeVariable.fresh()
          topLevelConstraint(t2) ++ genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, t2)

        case Let(ParamDef(name, TypeTree(typee)), value, body) =>
                  val evalType = TypeVariable.fresh()
                  //print(name.fullName)
                  topLevelConstraint(evalType) ++ genConstraints(value, typee) ++ genConstraints(body, evalType)(env + (name -> typee))

        case Ite(cond, thenn, elze) =>
          val bodyType = TypeVariable.fresh()
          genConstraints(cond, BooleanType) ++ genConstraints(thenn, bodyType) ++ genConstraints(elze, bodyType) ++ topLevelConstraint(bodyType)
        case Error(msg) =>
         // fatal(msg, msg.position)
          topLevelConstraint(expected) ++ genConstraints(msg, StringType)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          val bodyType = TypeVariable.fresh()

          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            pat match {
              case WildcardPattern() =>       (Nil, Map.empty)
              case IdPattern(name) =>         (Nil, Map(name -> scrutExpected))
              case LiteralPattern(l) =>       (genConstraints(l, scrutExpected), Map.empty)
              case CaseClassPattern(qname,args) =>
                val constrainArgs  : List[(List[Constraint], Map[Identifier, Type])] = args.map(argPattern => handlePattern(argPattern, TypeVariable.fresh()))
                val (argsConstrains, argsTypes) : (List[Constraint], Map[Identifier, Type]) = (constrainArgs.flatMap(_._1), constrainArgs.flatMap(_._2).toMap)
                (argsConstrains, argsTypes + (qname -> scrutExpected))
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)

            val bodyConstraints = genConstraints(cse.expr, bodyType)(env ++ moreEnv)
            patConstraints ++ bodyConstraints
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))


        case _ =>

          Nil  // TODO: Implement the remaining cases
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
     //println("----------------")
     //constraints.foreach(printmain
      // ln)

      constraints match {

        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found, expected) match {
           case (_, TypeVariable(ide)) =>
             solveConstraints(subst_*(more, ide, found))
           case(TypeVariable(ide), x )=>
             solveConstraints(Constraint(x, TypeVariable(ide),pos) :: more)
           case _ =>
             if(!found.toString.equals(expected.toString)){
               fatal(s"Type check error at:  $pos.toString. Expected ${expected.toString} and found ${found.toString}" )
             }
             solveConstraints(more)

          }


        }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
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
