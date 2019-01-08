package amyc
package parsing
import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._
import amyc.ast.NominalTreeModule
import ast.NominalTreeModule._
  import utils._
import amyc.analyzer._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}


object ASTPrecedenceCorrector  extends Pipeline[(N.Program, SymbolTable),(N.Program, SymbolTable)] {

  def run(ctx: Context)(v: (N.Program, SymbolTable)): (N.Program, SymbolTable) = {
    import ctx.reporter._
    val (p, table) = v

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
        case parent@N.OpCall(parentName, args) =>
          if(args.size == 2){
            val (leftNode, rightNode ) : (Option[N.OpCall], Option[N.OpCall]) = (isOp(args.head), isOp(args(1)))
            (leftNode, rightNode ) match {
              case (Some(left), right ) =>
                val recLeft = transformExpr(left)
                val parentRight = transformExpr(args(1))
                recLeft match {
                  case N.Call(leftOp, List(l,r)) =>
                    val (mod, name) = (leftOp.module, leftOp.name)
                    if(getOperatorPrecedence(leftOp) < getOperatorPrecedence(parentName)){
                      N.Call(leftOp, l :: N.Call(parentName, r :: parentRight :: Nil ) :: Nil)
                    }else {
                      N.Call(parentName, recLeft :: parentRight :: Nil)
                    }


                }

              case (None , _) =>
                N.Call(parentName, args.map(transformExpr(_)))
            }
          } else {
            fatal(s"Custom binary operator $parentName cannot have $args.size arguments", expr.position)
          }
        case N.Call(qname, args)      =>  N.Call(qname, args.map(transformExpr(_)))
        case N.Sequence(e1, e2)       =>  N.Sequence(transformExpr(e1), transformExpr(e2))
        case N.Let(df, value, body)   =>  N.Let(df, transformExpr(value), transformExpr(body))
        case N.Ite(cond, thenn, elze) =>  N.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        case N.Match(scrut, cases) =>
          N.Match(transformExpr(scrut), cases.map{case N.MatchCase(pat, expr) => N.MatchCase(pat, transformExpr(expr))})
        case _ =>
          expr


      }
    }

    def isOp(expr: N.Expr): Option[N.OpCall] = {
      expr match {
        case op@N.OpCall(_,_) => Some(op)
        case _ => None
      }
    }

    def getOperatorPrecedence(df: N.QualifiedName): Int = {
      df match {
        case N.QualifiedName(None, name) =>
          table.getOperator(name) match {
            case Some((_, OpSig(_,_,precedence))) =>
              precedence
            case None =>
              fatal(s"Operator $df is not in the symbol table")
          }
        case N.QualifiedName(None, name) =>
          fatal(s"Operator $name module not saved in the symbol table")
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
