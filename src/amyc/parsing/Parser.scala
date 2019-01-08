package amyc
package parsing

import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarDSL._
import grammarcomp.grammar.GrammarUtils.InLL1
import grammarcomp.grammar._
import grammarcomp.parsing._
import amyc.utils._
import ast.NominalTreeModule._
import Tokens._
import amyc.analyzer.SymbolTable

// The parser for Amy
// Absorbs tokens from the Lexer and then uses grammarcomp to generate parse trees.
// Defines two different grammars, a naive one which does not obey operator precedence (for demonstration purposes)
// and an LL1 grammar that implements the true syntax of Amy
//object Parser extends Pipeline[(Stream[Token], SymbolTable), Program] {
object Parser extends Pipeline[ Stream[Token], Program] {

  /* This grammar does not implement the correct syntax of Amy and is not LL1
   * It is given as an example
   */
  val amyGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id | 'Id ~ DOT() ~ 'Id,
    'Expr ::= 'Id | 'Literal | 'Expr ~ 'BinOp ~ 'Expr | BANG() ~ 'Expr | MINUS() ~ 'Expr |
              'QName ~ LPAREN() ~ 'Args ~ RPAREN() | 'Expr ~ SEMICOLON() ~ 'Expr |
              VAL() ~ 'Param ~ EQSIGN() ~ 'Expr ~ SEMICOLON() ~ 'Expr |
              IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              'Expr ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'Expr ~ RPAREN(),
    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
               AND() | OR() | EQUALS() | CONCAT(),
    'Cases ::= 'Case | 'Case ~ 'Cases,
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  // TODO: Write a grammar that implements the correct syntax of Amy and is LL1.
  // You can start from the example above and work your way from there.
  // Make sure you use the warning (see `run` below) that tells you which part is not in LL1.
  lazy val amyGrammarLL1 = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef |'OperatorDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,

    'FunDef ::=  DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE() ,
    'OperatorDef ::= OPERATOR() ~ INTLITSENT ~  DEF() ~ 'OpDefId ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type  ~ 'OptionalBody ,
    'Operator ::= OPLITSENT,
    'OpDefId::= OPLITSENT |  PLUS() | MINUS() | DIV() | TIMES()  | OR() | AND() | LESSTHAN() | MOD() | LESSEQUALS() | CONCAT() | EQUALS(),
    'OptionalBody ::=    EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE() | epsilon(),

    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param  ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'OptOp ::=  OPERATOR() | epsilon(),

    'ExprTail ::= SEMICOLON() ~ 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id ~ 'QNames,
    'QNames ::= DOT() ~ 'Id | epsilon(),
    'Expr ::= VAL() ~ 'Param ~ EQSIGN() ~ 'ExprTerm ~ SEMICOLON() ~  'Expr  |'ExprTerm ~ 'ExprTail,

    //'ExprTerm ::= 'OptMatch ~ 'OrTerm ,
    'ExprTerm ::= 'LastLevelTerm ~ 'OptMatch,
    'OptMatch ::= MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE()| epsilon(),
    /*'OrTermList ::=  OR() ~  'OrTerm  | epsilon(),
    'OrTerm ::= 'AndTerm ~ 'OrTermList,

    'AndTermList ::= AND() ~ 'AndTerm | epsilon(),
    'AndTerm ::=  'EqTerm ~ 'AndTermList,

    'EqTermList ::= EQUALS() ~'EqTerm | epsilon(),
    'EqTerm ::= 'LessTerm ~ 'EqTermList,

    'LessTermList ::= 'LESS ~ 'LessTerm| epsilon(),
    'LessTerm ::= 'Plus_MinusTerm ~ 'LessTermList,

    'Plus_MinusTermList ::= 'PLUS_MINUS ~ 'Plus_MinusTerm | epsilon(),
    'Plus_MinusTerm ::='MUL_DIV_MODTerm ~ 'Plus_MinusTermList,

    'MUL_DIV_MODTermList ::= 'MUL_DIV_MOD ~ 'MUL_DIV_MODTerm | epsilon(),
    'MUL_DIV_MODTerm ::= 'LastLevelTerm ~ 'MUL_DIV_MODTermList,
    */
    'LastLevelList ::=  'OpDefId ~ 'LastLevelTerm | epsilon(),
    'LastLevelTerm ::= 'FinalTerm ~ 'LastLevelList,



    'FinalTerm ::= 'If | 'Error | 'Id ~ 'OptCall |'LiteralNoEmptyPar | 'EmptyParOrParExpr
      | BANG() ~ 'FinalTerm | MINUS() ~ 'FinalTerm ,

    'OptForQname ::= DOT() ~ 'Id | epsilon(),
    'OptForUnary ::= 'UNARY | epsilon(),
    'OptCall ::= 'OptForQname ~ LPAREN() ~ 'Args ~ RPAREN() | epsilon(),
    'ParExpr ::=  LPAREN() ~ 'Expr ~ RPAREN(),
    'Val ::= 'Id,
    'Call ::= 'QName ~ LPAREN() ~ 'Args ~ RPAREN(),
   // 'OperatorCall ::=  OPLITSENT ~ 'Expr,
    'Error ::= ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN(),
    'If ::= IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE(),

    'UNARY ::=  'Operator ~ 'FinalTerm,
    //UNARY ::= 'Id ~ 'FinalTerm,
    'PLUS_MINUS ::= PLUS() | MINUS() | CONCAT(),
    'MUL_DIV_MOD ::= TIMES() | DIV() | MOD(),
    'LESS ::= LESSTHAN() | LESSEQUALS(),

    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'LiteralNoEmptyPar ::=  TRUE() | FALSE() | INTLITSENT | STRINGLITSENT,
    'EmptyParOrParExpr ::= LPAREN() ~ 'OptExpr  ~ RPAREN(),


    'Cases ::= 'Case ~ 'CasesTail,
    'CasesTail ::=  'Cases | epsilon(),
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE()  | 'IdOrQnameWithPattern | 'Literal ,

    'IdOrQnameWithPattern ::= 'QName ~ 'OptPatCall,
    'Patterns ::= 'Pattern ~ 'PatternList,
    'OptPattern ::= 'Pattern | epsilon(),

    'OptPatCall ::= LPAREN() ~ 'OptPatterns  ~ RPAREN()| epsilon(),
    'OptPatterns ::= 'Patterns | epsilon(),
    'PatternList ::= epsilon() | COMMA() ~'Patterns,

    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  //def run(ctx: Context)(tokens: Stream[Token], table:SymbolTable): Program = {
  def run(ctx: Context)(tokens: Stream[Token]): Program = {
    // TODO: Switch to LL1 when you are ready
    val (grammar, constructor) = (amyGrammarLL1, new ASTConstructorLL1)
   // val (grammar, constructor) = (amyGrammar, new ASTConstructor)

    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()

    GrammarUtils.isLL1WithFeedback(amyGrammarLL1) match {
      case InLL1() =>
        info("Grammar is in LL1")
      case other =>
        warning(other)
    }

    val feedback = ParseTreeUtils.parseWithTrees(grammar, tokens.toList)
    feedback match {
      case s: Success[Token] =>
        constructor.constructProgram(s.parseTrees.head)
      case err@LL1Error(_, Some(tok)) =>
        fatal(s"Parsing failed: $err", tok.obj.position)
      case err =>
        fatal(s"Parsing failed: $err")
    }
  }

}
