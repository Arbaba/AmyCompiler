package amyc
package parsing
import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._
import amyc.ast.NominalTreeModule
import ast.NominalTreeModule._
  import utils._
import amyc.analyzer.SymbolTable
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}


object ASTPrecedenceCorrector  extends Pipeline[(N.Program, SymbolTable),N.Program] {

  def run(ctx: Context)(p: N.Program, table: SymbolTable): N.Program = {
    import ctx.reporter._

    def transformOpAndFunDef(expr: N.ClassOrFunDef)(implicit module: String): N.ClassOrFunDef = {
      expr match {
        case N.FunDef(name, params, retType, body) =>
          N.FunDef(name,params, retType, transformExpr(body))
        case N.OpDef(name, params, retType, body, precedence) =>
          N.OpDef(name, params, retType, transformExpr(body), precedence)
        case x => x
      }
    }
    def transformExpr(expr: N.Expr)(implicit module:String): N.Expr = {
      expr match {
        case N.OpCall(name, args) =>
          ???

      }
    }
    val newProgram = N.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        N.ModuleDef(
          name,
          defs map (transformOpAndFunDef(_)(name)) ,
          optExpr map (transformExpr(_)(name))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)


  }
}
