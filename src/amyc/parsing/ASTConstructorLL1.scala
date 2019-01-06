package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._
import amyc.ast.NominalTreeModule
import ast.NominalTreeModule._
import amyc.analyzer.SymbolTable

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  // TODO: Override methods from ASTConstructor as needed

  /* ... */


  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
   def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {

   ptree match {
     case Node(_, List()) => //epsilon rule of the nonterminals
       leftopd
     case Node(sym ::= _, List(op, rightNode))
       //if Set('OrExpr, 'AndExpr, 'EqExpr, 'CompExpr, 'AddExpr, 'MultExpr) contains sym =>
       if Set('ExprTerm, 'OrTermList, 'AndTermList, 'EqTermList, 'LessTermList, 'Plus_MinusTermList, 'MUL_DIV_MODTermList, 'LastLevelList) contains sym =>
     rightNode match {
         case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
           val nextAtom = findAndUseExpr(nextOpd)

           op match {
             case Node('Operator ::=_, List(Leaf(OPLIT(operator)))) =>
               constructOpExpr(OpCall(QualifiedName(Some("Arithmetic"), operator), List(leftopd, nextAtom)), suf)
             case _ =>
               constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd),  suf)

           }
            // captures left associativity
         case Node(_, List()) =>
         leftopd
       }
   }


 }

  override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    ptree match {
      case Node(_, List(Leaf(t))) =>
        tokenToExpr(t)
      case Node('Less ::=_, List(Leaf(LESSTHAN())))=>
        LessThan
      case Node('Less ::=_, List(Leaf(LESSEQUALS())))=>
        LessEquals
      case Node ('PLUS_MINUS ::=_ , List(Leaf(PLUS()))) =>
        Plus
      case Node ('PLUS_MINUS ::=_ , List(Leaf(MINUS()))) =>
        Minus
      case Node ('PLUS_MINUS ::=_ , List(Leaf(CONCAT())) )=>
        Concat
      case Node('MUL_DIV_MODTerm ::=_, List(Leaf(TIMES()))) =>
        Times
      case Node ('MUL_DIV_MODTerm ::=_ , List(Leaf(DIV()))) =>
        Div
      case Node('MUL_DIV_MODTerm ::=_, List(Leaf(MOD())) )=>
        Mod

      case Leaf(op) => tokenToExpr(op)
    }
  }


   /*override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) ={
     pTree match {
       case Node('QName ::= _, List(id, qnames)) =>
         val (name, pos) = constructName(id)
         val tail = constructQnames(qnames)
         tail match {
           case Some((string, position)) =>
             (QualifiedName(Some(name), string), pos)
           case None =>
             (QualifiedName(None, name), pos)
         }
     }

   }*/

 def constructQnames(pTree: NodeOrLeaf[Token]): Option[(String, Positioned)] = {
   pTree match {
     case Node('Qnames ::= List(DOT(), 'Id), List(_, id) ) =>
       Some(constructName(id))
     case Node('Qnames ::= _, List()) =>
       None
   }
 }


  override def constructDef0(pTree: NodeOrLeaf[Token]): ClassOrFunDef = {
    pTree match {
      case Node('OperatorDef ::=_, List(Leaf(operator), Leaf(it@INTLIT(precedence)),  Leaf(df), name, _, params, _, _, retType, _, _, body, _)) =>
        OpDef(constructOperatorName(name)._1,
          constructList(params, constructParam, hasComma = true),
          constructType(retType),
          constructExpr(body),
          precedence
        ).setPos(operator)
      case _ =>
        super.constructDef0(pTree)
    }
  }


  def constructOperatorName(ptree: NodeOrLeaf[Token]): (String, Positioned) = {
    ptree match {
      case Node('Operator ::= _, List(Leaf(id@OPLIT(name)))) =>
        (name, id)
    }
  }



  override def constructCase(pTree: NodeOrLeaf[Token]): MatchCase = {
    pTree match {
      case Node('Case ::= _, List(Leaf(ct), pat, _, expr)) =>
        MatchCase(constructPattern(pat), constructExpr(expr)).setPos(ct)
    }
  }

  override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)

      case Node('Pattern ::= List('IdOrQnameWithPattern), List(child)) =>
        constructIdOrQnameWPattern(child)


    }
  }

  def constructIdOrQnameWPattern(ptree : NodeOrLeaf[Token]): Pattern = {
    ptree match {
      case Node('IdOrQnameWithPattern::= List('QName, 'OptPatCall), List(qname, optPatCall)) =>
        optPatCall match {
          case Node('OptPatCall::= _, List(Leaf(lparen), optPatterns, _)) =>
            optPatterns match {
              case Node('OptPatterns ::= _, List(patterns)) =>
                CaseClassPattern(constructQname(qname)._1, constructPatterns(patterns))//constr(args
              case Node('OptPatterns ::=_ , List())=>
                CaseClassPattern(constructQname(qname)._1, Nil) // constr()
            }
          case Node('OptPatCall::= _, List()) =>
            qname match {
              case Node('QName::= _, List(id, tail)) =>
                val (qn, pos) = constructQname(qname)
                IdPattern(qn.name).setPos(pos) //Id simple
            }

        }

    }
  }

  def constructPatterns(ptree : NodeOrLeaf[Token]): List[Pattern] = {
    ptree match {
      case Node('Patterns::=_, List(pattern, patternsList)) =>
        patternsList match {
          case Node('PatternList ::= List(COMMA(), 'Patterns ), List(Leaf(comma), patterns)) =>
            constructPattern(pattern) :: constructPatterns(patterns)
          case Node('PatternList ::= _, List()) =>
            List(constructPattern(pattern))
        }
      case Node('Patterns::=_, List()) =>
        Nil
    }
  }



  override def constructExpr(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {

   ptree match {
     case Node('Expr ::= List(VAL() , 'Param ,EQSIGN() , 'ExprTerm , SEMICOLON() ,'Expr),
               List(Leaf(vall), par, Leaf(_), exprTerm, Leaf(_), expr)) =>
       val paramDef = constructParam(par)
       Let(paramDef, constructExprTerm(exprTerm), constructExpr(expr)).setPos(vall)//SET POS?????

     case Node('Expr ::= List('ExprTerm, 'ExprTail), List(term, tail)) =>
       tail match {
         case Node('ExprTail ::= List(SEMICOLON(), 'Expr), List(Leaf(pos), expr)) =>
           Sequence(constructExprTerm(term), constructExpr(expr))
         case Node('ExprTail ::= _, List()) =>
           constructExprTerm(term)
       }
   }
 }
  def findAndUseExpr(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr ={
    ptree match {
      case Node(exprtype ::=_,_ ) =>
        exprtype match {
          case 'Expr => constructExpr(ptree)
          case 'ExprTerm => constructExprTerm(ptree)
          case 'OrTerm => constructOrTerm(ptree)
          case 'AndTerm => constructAndTerm(ptree)
          case 'EqTerm => constructEqTerm(ptree)
          case 'LessTerm => constructLessTerm(ptree)
          case 'Plus_MinusTerm => constructPlusMinusTerm(ptree)
          case 'MUL_DIV_MODTerm => constructMulDivModTerm(ptree)
          case 'FinalTerm => constructFinalTerm(ptree)
          case 'LastLevelTerm => constructLastLevelTerm(ptree)
        }
    }
  }

   def constructExprTerm(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match {
      case Node('ExprTerm ::= List('OrTerm, 'OptMatch), List(scrut, optMatch)) =>
        optMatch match {
          case Node('OptMatch ::= List(MATCH(), LBRACE(), 'Cases ,RBRACE()), List(Leaf(matchh),_, cases,_)) =>
            Match(constructOrTerm(scrut), constructCases(cases))//construct cases
          case Node('OptMatch ::= _, List()) =>
            constructOrTerm(scrut)
        }
    }
  }

  def constructCasesTail(ptree: NodeOrLeaf[Token]): List[NominalTreeModule.MatchCase] = {
    ptree match {
      case Node('CasesTail ::= List('Cases), List(cases)) => constructCases(cases)
      case Node('CasesTail ::= _, List()) => Nil
    }
  }

  def constructCases(ptree: NodeOrLeaf[Token]): List[NominalTreeModule.MatchCase] = {
    ptree match {
      case Node('Cases ::= List('Case, 'CasesTail), List(casee, tail)) =>
        super.constructCase(casee) :: constructCasesTail(tail)
    }
  }
  def constructOrTerm(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match  {
      case Node('OrTerm ::=_, List(andterm, ortermlist)) =>
        ortermlist match {
          case Node('OrTermList ::= (OR() ::_), List(or,orterm)) =>
            constructOpExpr(constructAndTerm(andterm), ortermlist)
          case Node(_, List()) =>
            constructAndTerm(andterm)
        }
    }
  }

  def constructAndTerm(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match  {
      case Node('AndTerm ::=_, List(eqterm, andtermlist)) =>
        andtermlist match {
          case Node('AndTermList ::= (AND() :: _), List(and,andterm)) =>
            //constructOpExpr(and)(eqterm, andterm)
            constructOpExpr(constructEqTerm(eqterm), andtermlist)
          case Node(_, List()) =>
            constructEqTerm(eqterm)
        }
    }
  }

  def constructEqTerm(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match  {
      case Node('EqTerm ::=_, List(lessterm, eqtermlist)) =>
        eqtermlist match {
          case Node('EqTermList ::= List(EQUALS(),'EqTerm), List(eq,eqterm)) =>
            constructOpExpr(constructLessTerm(lessterm), eqtermlist)
          case Node(_, List()) =>
            constructLessTerm(lessterm)
        }
    }
  }

  def constructLessTerm(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match  {
      case Node('LessTerm ::=_, List(plusminusterm, lesstermlist)) =>
        lesstermlist match {
          case Node('LessTermList ::= List('LESS ,_), List(less,lessterm)) =>
            less match {
              case Node('LESS ::= _, List(ltorless)) =>
                constructOpExpr(constructPlusMinusTerm(plusminusterm), lesstermlist)
            }
          case Node(_, List()) =>
            constructPlusMinusTerm(plusminusterm)
        }
    }
  }

  def constructPlusMinusTerm(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match  {
      case Node('Plus_MinusTerm ::= List('MUL_DIV_MODTerm,'Plus_MinusTermList), List(muldivmodterm, plusminustermlist)) =>
        plusminustermlist match {
          case Node('Plus_MinusTermList ::= List('PLUS_MINUS,_), List(plusminus,plusminusterm)) =>
            plusminus match {
              case Node('PLUS_MINUS ::= _, List(op)) =>
                constructOpExpr(constructMulDivModTerm(muldivmodterm), plusminustermlist)
            }
          case Node(_, List()) =>
            constructMulDivModTerm(muldivmodterm)
        }
    }
  }


def constructMulDivModTerm(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
  ptree match  {
    case Node('MUL_DIV_MODTerm  ::= List('LastLevelTerm, 'MUL_DIV_MODTermList), List(lastLevel, muldivmodtermlist)) =>
      muldivmodtermlist match {
        case Node('MUL_DIV_MODTermList::= List('MUL_DIV_MOD ,_), List(mdv, muldivmodterm)) =>
          mdv match {
            case Node('MUL_DIV_MOD ::= _, List(op)) =>
              constructOpExpr(constructLastLevelTerm(lastLevel),muldivmodtermlist )
          }
        case Node(_, List()) =>
          constructLastLevelTerm(lastLevel)
      }
  }
}


  def constructLastLevelTerm(pTree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    pTree match {
      case Node('LastLevelTerm ::= List('FinalTerm, 'LastLevelList),List(finalTerm, lastLevelList)) =>
        lastLevelList match {
          case Node('LastLevelList ::= List('Operator,_), List(operator, lastLevelterm)) =>
            operator match {
              case Node('Operator ::= _, List(op)) =>
                constructOpExpr(constructFinalTerm(finalTerm), lastLevelList)
            }
          case Node(_, List()) =>
            constructFinalTerm(finalTerm)
        }
    }
  }
// Need to add  Minus
def constructFinalTerm(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
 // println(ptree)
  ptree match {
    case Node('FinalTerm ::= List('LiteralNoEmptyPar), List(lit)) =>
      constructLiteral(lit)
    case Node('FinalTerm ::= List('If), List(iff)) =>
      constructIf(iff)
    case Node('FinalTerm ::= ('Error ::_), List(err)) =>
      constructError(err)
    case Node('FinalTerm ::= ( 'Id :: 'OptCall ::_), List(name, optcall)) =>
      optcall match {
        case Node('OptCall ::= _, List()) =>
          val (id,pos) = constructName(name)
          Variable(id).setPos(pos)
        case Node('OptCall ::= ('OptForQname :: LPAREN() :: 'Args :: RPAREN() :: _), List(optqname, _, par, _)) =>
          val args = constructList(par, constructExpr, hasComma = true)
          solveOptQname(optqname) match {
            case None =>  //no module
              val (identifier, pos) = constructName(name)
              Call(QualifiedName(None, identifier), args).setPos(pos)
            case Some(Variable(identifier)) => //module
              val (module, modulePos) = constructName(name)
              Call(QualifiedName(Some(module), identifier), args).setPos(modulePos)
          }
      }

    case Node('FinalTerm ::= List('EmptyParOrParExpr), List(unitOrExpr)) =>
      constructEmptyParOrParExpr(unitOrExpr)
   /* case Node('FinalTerm ::= ('EmptyParOrParExpr ::_), List(foo)) =>
      constructEmptyParOrParExpr(foo)*/
    case Node('FinalTerm ::= (BANG() :: 'FinalTerm ::_), List(Leaf(justForPos), finalTerm)) =>
     Not(constructFinalTerm(finalTerm)).setPos(justForPos)
    case Node('FinalTerm ::= (MINUS() :: 'FinalTerm ::_), List(Leaf(justForPos), finalTerm)) =>
      Neg(constructFinalTerm(finalTerm)).setPos(justForPos)
    case Node('FinalTerm ::=List('Operator, 'FinalTerm), List(operator, finalTerm))=>
      operator match {
        case Node('Operator ::=_ , List(Leaf(o@OPLIT(op)))) =>
          OpCall(QualifiedName(None,op), constructFinalTerm(finalTerm) :: Nil).setPos(o)
      }
  }
}

  def constructEmptyParOrParExpr(ptree : NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match {
      case Node('EmptyParOrParExpr ::= _, List(Leaf(lpar), optExpr,_) ) =>
        constructOptExpr(optExpr)

    }
  }

  def constructOptExpr(ptree : NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match {
      case Node('OptExpr ::= List('Expr), List(expr)) =>
        constructExpr(expr)
      case Node('OptExpr ::= _, List()) =>
        UnitLiteral()
    }
  }

  def isEpsilon(ptree: NodeOrLeaf[Token]): Boolean = {
    ptree match {
      case Node(sym ::= _, List()) => true
      case Node(_, _) => false
      case Leaf(_) => false

    }
  }

  def solveOptQname(ptree: NodeOrLeaf[Token]): (Option[Variable]) = {
    ptree match {
      case Node(sym ::= _, List()) => None
      case Node('OptForQname ::= List(DOT(), 'Id), List(_, id)) =>
        val (name, pos) = constructName(id)
        Some(Variable(name).setPos(pos))
    }

  }

  def constructIf(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match {
      case Node('If ::= List(IF() , LPAREN() , 'Expr , RPAREN() , LBRACE() , 'Expr , RBRACE() , ELSE() , LBRACE() , 'Expr , RBRACE()), List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)) =>
        Ite(
          constructExpr(cond),
          constructExpr(thenn),
          constructExpr(elze)
        ).setPos(it)
    }

  }

  def constructError(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match {
      case Node('Error ::= (ERROR() :: _ ::_::_::_), List(Leaf(ert), _, msg, _)) =>
        Error(constructExpr(msg)).setPos(ert)
    }

  }
  override def constructLiteral(pTree: NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node('LiteralNoEmptyPar ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('LiteralNoEmptyPar ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('LiteralNoEmptyPar ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('LiteralNoEmptyPar ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
      case Node('LiteralNoEmptyPar ::= _, List(Leaf(lp@LPAREN()), Leaf(RPAREN()))) =>
        UnitLiteral().setPos(lp)
      case _ => super.constructLiteral(pTree)
    }
  }
  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match {
       case Node('QName ::= List('Id,'QNames), List(modOrId, qnames)) =>
         qnames match {
           case Node('QNames ::= List(DOT(), 'Id), List(Leaf(dot), id)) =>
             val (module, pos) = constructName(modOrId)
             val (name, _) = constructName(id)
             (QualifiedName(Some(module), name), pos)
           case Node('QNames ::= _, List()) =>
             val (name, pos) = constructName(modOrId)
             (QualifiedName(None, name), pos)
         }
      case Node('FinalTerm ::= ( 'Id :: 'OptForQname :: LPAREN() :: 'Args :: RPAREN()::_),  List(modOrId, qnames, _, args,_)) =>
        qnames match {
          case Node('OptForQname ::= (DOT():: 'Id:: _), List(_, id)) =>
            val (module, pos) = constructName(modOrId)
            val (name, _) = constructName(id)
            (QualifiedName(Some(module), name), pos)
          case Node('OptForQname ::= List(), _) =>
            val (name, pos) = constructName(modOrId)
            (QualifiedName(None, name), pos)
        }
    }
  }

  def constructExprList(pTree: NodeOrLeaf[Token]): List[NominalTreeModule.Expr] = {
    pTree match {
      case Node('ExprList ::= (COMMA() :: 'Expr :: 'ExprList :: _), (expr :: exprLists:: _)) =>
        constructList(pTree, constructExpr)
      case Node('ExprList ::= List(), _) =>
        Nil
    }
  }

  def constructArgs(pTree: NodeOrLeaf[Token]): List[NominalTreeModule.Expr] = {
    pTree match {
      case Node('Args ::= ('Expr :: 'ExprList :: _), (expr :: exprLists:: _)) =>
        constructList(pTree, constructExpr)
      case Node('ExprList ::= (COMMA() :: 'Expr :: 'ExprList::_), (Leaf(pos) :: expr :: exprList ::_)) =>
        constructExpr(expr) :: constructExprList(exprList)
    }
  }
}

