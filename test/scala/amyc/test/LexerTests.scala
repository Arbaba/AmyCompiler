package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "lexer"

  val outputExt = "txt"

  //@Test def testKeywords = shouldOutput("Keywords")

	@Test def testLiterals = shouldOutput("Literals")

  @Test def testSingleAmp = shouldFail("SingleAmp")

}
