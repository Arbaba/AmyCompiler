package amyc
package codegen

import analyzer._
import ast.Identifier
import ast.SymbolicTreeModule.{Call => AmyCall, Div => AmyDiv, And => AmyAnd, Or => AmyOr, _}
import utils.{Context, Pipeline}
import wasm._
import Instructions._
import Utils._

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[(Program, SymbolTable), Module] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v
		import ctx.reporter._

	val seq = (a: Code, b: Code) => a <:> b
    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect {
				case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) => cgFunction(fd, name, false)
				case od: OpDef => cgOperator(od)
      } ++
      // Generate code for the "main" function, which contains the module expression
      optExpr.toList.map { expr =>
        val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
        cgFunction(mainFd, name, true)
      }
    }

		def cgOperator(od: OpDef): Function = {
      // Note: We create the wasm function name from a combination of
      // module and function name, since we put everything in the same wasm module
			println(s"define ${od.name}")
      Function(od.name.name, od.params.size, false){ lh =>
        val locals = od.paramNames.zipWithIndex.toMap
        cgExpr(od.body)(locals, lh)
      }
    }

    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the wasm function name from a combination of
      // module and function name, since we put everything in the same wasm module.
      val name = fullName(owner, fd.name)
      Function(name, fd.params.size, isMain){ lh =>
        val locals = fd.paramNames.zipWithIndex.toMap
        val body = cgExpr(fd.body)(locals, lh)
        if (isMain) {
          body <:> Drop // Main functions do not return a value,
                        // so we need to drop the value generated by their body
        } else {
          body
        }
      }
    }

    // Generate code for an expression expr.
    // Additional arguments are a mapping from identifiers (parameters and variables) to
    // their index in the wasm local variables, and a LocalsHandler which will generate
    // fresh local slots as required.
    def cgExpr(expr: Expr)(implicit locals: Map[Identifier, Int], lh: LocalsHandler): Code = {
			def loadOperands(lhs: Expr, rhs: Expr): Code = cgExpr(lhs) <:> cgExpr(rhs)
			expr match {
				case Variable(n) => GetLocal(locals(n))
				case IntLiteral(i) => Const(i)
				case BooleanLiteral(b) => Const(if(b) 1 else 0)
				case StringLiteral(s) => mkString(s)
				case UnitLiteral() => Const(0)
				case Plus(lhs, rhs) => loadOperands(lhs, rhs) <:> Add
				case Minus(lhs, rhs) => loadOperands(lhs, rhs) <:> Sub
				case Times(lhs, rhs) => loadOperands(lhs, rhs) <:> Mul
				case AmyDiv(lhs, rhs) => loadOperands(lhs, rhs) <:> Div
				case Mod(lhs, rhs) => loadOperands(lhs, rhs) <:> Rem
				case LessThan(lhs, rhs) =>
					loadOperands(lhs, rhs) <:>
					Lt_s
				case LessEquals(lhs, rhs) => loadOperands(lhs, rhs) <:> Le_s
				case AmyAnd(lhs, rhs) => loadOperands(lhs, rhs) <:> And
				case AmyOr(lhs, rhs) => loadOperands(lhs, rhs) <:> Or
				case Equals(lhs, rhs) => loadOperands(lhs, rhs) <:> Eq
				case Concat(lhs, rhs) =>
					loadOperands(lhs, rhs) <:>
					Call("String_concat")
				case Not(e) => cgExpr(e) <:> If_i32 <:> Const(0) <:> Else <:> Const(1) <:> End
				case Neg(e) =>
					Const(0) <:>
					cgExpr(e) <:>
					Sub
				case Ite(i, t, e) =>
					cgExpr(i) <:>
					If_i32 <:>
					cgExpr(t) <:>
					Else <:>
					cgExpr(e) <:>
					End
				case AmyCall(qname, args) =>
					val func = program.modules.filter { case mod: ModuleDef => mod.defs.map(_.name).exists(id => id.toString == qname.name.toString) }
					(table getConstructor qname, table getFunction qname, table getOperator qname) match {
						case (Some(ConstrSig(argTypes, _, idx)), _, _) =>
							fatal("ADT constructors not implemented")
						case (_, Some(_), _) =>
							(args map cgExpr reduceLeft seq) <:>
							Call(func(0).name.toString + "_" + qname.name)
						case (_, _, Some(_)) =>
							(args map cgExpr reduceLeft seq) <:>
							Call(qname.name)
					}
				case Let(ParamDef(name, tt), v, bdy) =>
					val i = lh.getFreshLocal
					cgExpr(v) <:>
					SetLocal(i) <:>
					cgExpr(bdy)(locals + (name -> i), lh)
				case Error(err) => mkString("Error:	") <:> cgExpr(err) <:> Call("Std_printString") <:> Unreachable
				/*case Match(scrut, cases) =>
					val scruti = lh.getFreshLocal()
					cgExpr(scrut) <:>
					SetLocal(scruti) <:> // @scrut
					matchAndBind(cases, scruti)*/
				case Sequence(e1, e2) => cgExpr(e1) <:> cgExpr(e2)
			}
    }

    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }
}
