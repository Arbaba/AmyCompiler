package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

/**

- Discover all operators with their precedence
- Checks that all operators have right arity
- Keeps the tree in nominal form
- We do not need to modify Call because the precedence should live in the SymbolTable anyway

**/
//The main difference with NameAnalyzer is that this one keeps the tree in Nominal form
//It can be argued that NameAnalyzer is not needed anymore, we could reform the tree in symbolic form
object PreAnalyzer extends Pipeline[N.Program, (N.Program, SymbolTable)] {

	def run(ctx: Context)(p: N.Program): (N.Program, SymbolTable) = {
		import ctx.reporter._
		val operatorsTable = new SymbolTable
		def transformType(tt: N.TypeTree, inModule: String): S.Type = {
	    tt.tpe match {
	      case N.IntType => S.IntType
	      case N.BooleanType => S.BooleanType
	      case N.StringType => S.StringType
	      case N.UnitType => S.UnitType
	      case N.ClassType(qn@N.QualifiedName(module, name)) =>
	        operatorsTable.getType(module getOrElse inModule, name) match {
	          case Some(symbol) =>
	            S.ClassType(symbol)
	          case None =>
	            fatal(s"Could not find type $qn in $inModule", tt)
	        }
	    }
	  }
		//Check modules uniqueness
		val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }
		//Add modules to the table
    modNames.keys.toList foreach operatorsTable.addModule
		//Check names uniqueness (of operators, functions, types, ...)
    modNames foreach {
			case (name, module :: Nil) =>
			module.defs groupBy (_.name) foreach {
				case (name, definitions) => if(definitions.size > 1) fatal(s"$name is defined ${definitions.size} times")
			}
		}
		//Discover operator definitions
		for {
			(owner, mod :: Nil) <- modNames
			N.OpDef(name, param, ret, bdy, precedence) <- mod.defs
		} operatorsTable addOperator (owner, name, param map (_.tt) map { case tree: N.TypeTree => transformType(tree, owner)},transformType(ret, owner), precedence)
		//Check operators
		def checkArity(expr: N.Expr): Unit = expr match {
			case N.Match(scrut, cases) =>
				checkArity(scrut)//; cases foreach checkArity
			case N.Plus(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.Minus(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.Times(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.Div(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.Mod(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.LessThan(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.LessEquals(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.And(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.Or(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.Equals(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.Concat(lhs, rhs) => checkArity(lhs); checkArity(rhs)
			case N.Let(pdef, value, bdy) => checkArity(value); checkArity(bdy)
			case N.Not(expr) => checkArity(expr)
			case N.Neg(expr) => checkArity(expr)
			case N.Sequence(expr1, expr2) => checkArity(expr1); checkArity(expr2)
			case N.Ite(i, t, e) => checkArity(i); checkArity(t); checkArity(e)
			case N.Error(msg) => checkArity(msg)
			case N.Call(qname, args) => {}
				//if(args.size != operatorsTable.getOperator(qname)) fatal(s"$qname has ${args.size} arguments, should have 2")
			case _ => {}
		}

    p.modules foreach { case mod@N.ModuleDef(name, defs, optExpr) =>
        optExpr foreach checkArity
    }

		(p, operatorsTable)
	}
}
