package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

/**

- Discover all operators with their precedence
- Checks that all operators have right arity
- Keeps the tree in nominal form

**/

object PreAnalyzer extends Pipeline[N.Program, (N.Program, SymbolTable)] {
	def run(ctx: Context)(p: N.Program): (N.Program, SymbolTable) {
		val operatorsTable = new SymbolTable
		(p, operatorsTable)
	}
}
