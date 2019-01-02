package amyc
package parsing

import utils._
import scala.io.Source
import java.io.File

// The lexer for Amy.
// Transforms an iterator coming from scala.io.Source to a stream of (Char, Position),
// then uses a functional approach to consume the stream.
object Lexer extends Pipeline[List[File], Stream[Token]] {
  import Tokens._
	// Special character which represents the end of an input file
  val EndOfFile: Char = scala.Char.MaxValue

	type Input = (Char, Position)
/** Maps a string s to the corresponding keyword,
  * or None if it corresponds to no keyword
  */
  private def keywords(s: String): Option[Token] = s match {
    case "abstract" => Some(ABSTRACT())
    case "Boolean"  => Some(BOOLEAN())
    case "case"     => Some(CASE())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "else"     => Some(ELSE())
    case "error"    => Some(ERROR())
    case "extends"  => Some(EXTENDS())
    case "false"    => Some(FALSE())
    case "if"       => Some(IF())
    case "Int"      => Some(INT())
    case "match"    => Some(MATCH())
    case "object"   => Some(OBJECT())
    case "String"   => Some(STRING())
    case "true"     => Some(TRUE())
    case "Unit"     => Some(UNIT())
    case "val"      => Some(VAL())
    case _          => None
  }

  private def lexFile(ctx: Context)(f: File): Stream[Token] = {

    val source = Source.fromFile(f)
		 // Useful type alias:
				    // The input to the lexer will be a stream of characters,
				    // along with their positions in the files

				    def mkPos(i: Int) = Position.fromFile(f, i)

    // The input to the lexer
    val inputStream: Stream[Input] =
      source.toStream.map(c => (c, mkPos(source.pos))) #::: Stream((EndOfFile, mkPos(source.pos)))

		lexStream(ctx)(inputStream)
	}


	def lexStream(ctx: Context)(inputStream: Stream[Input]): Stream[Token] = {
    import ctx.reporter._



    /** Gets rid of whitespaces and comments and calls readToken to get the next token.
      * Returns the first token and the remaining input that did not get consumed
      */
    @scala.annotation.tailrec
    def nextToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)
      val (currentChar, currentPos) #:: rest = stream
      // Use with care!
      def nextChar = rest.head._1
      if (Character.isWhitespace(currentChar)) {
        nextToken(stream.dropWhile{ case (c, _) => Character.isWhitespace(c) } )
      } else if (currentChar == '/' && nextChar == '/') {
        // Single-line comment
				nextToken(stream.dropWhile{ case(c, _) => c != '\n' && c != EndOfFile})
      } else if (currentChar == '/' && nextChar == '*') {
        // Multi-line comment
				def consume(stream: Stream[Input]): Stream[Input] = stream match {
					case (x1, _) #:: (x2, _) #:: xs if(x1 == '*' && x2 == '/') => xs
					case (EndOfFile, lastIdx) #:: xs  => {
						ctx.reporter.fatal("Unended comment")
					}
					case _ #:: tail => consume(tail)
				}
				nextToken(consume(rest.tail))
      } else {
        readToken(stream)
      }
    }
//8c2683f53ad01863ee5244c98244ec5b
    /** Reads the next token from the stream. Assumes no whitespace or comments at the beginning.
      * Returns the first token and the remaining input that did not get consumed.
      */
    def readToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream
      // Use with care!
      def nextChar = rest.head._1

			def isStringChar(c: Character): Boolean = c != '\n' && c != '"' && c != EndOfFile

      // Returns input token with correct position and uses up one character of the stream
      def useOne(t: Token) = (t.setPos(currentPos), rest)
      // Returns input token with correct position and uses up two characters of the stream
      def useTwo(t: Token) = (t.setPos(currentPos), rest.tail)
      currentChar match {
        case `EndOfFile` => useOne(EOF())

        // Reserved word or Identifier
        case _ if Character.isLetter(currentChar) =>
          val (wordLetters, afterWord) = stream.span { case (ch, _) =>
            Character.isLetterOrDigit(ch) || ch == '_'
          }
          val word = wordLetters.map(_._1).mkString
          // Hint: Decide if it's a letter or reserved word (use our infrastructure!),
          // and return the correct token, along with the remaining input stream.
          // Make sure you set the correct position for the token.
					//println(word)
					(keywords(word) getOrElse ID(word) setPos currentPos, afterWord)


        // Int literal
        case _ if Character.isDigit(currentChar) =>
          // Hint: Use a strategy similar to the previous example.
          // Make sure you fail for integers that do not fit 32 bits.
          val (digits, afterNumber) = stream span { case (dig, _) =>
						Character isDigit dig
					}
					//to string and then parseInt
					val numbers = digits.map(_._1.toString).foldLeft("")((x:String, y:String) => x + y)
					try {
			        (INTLIT(numbers.toInt) setPos currentPos, afterNumber)
			    } catch {
			        case e: NumberFormatException => {
								ctx.reporter.fatal("Number format")
								(BAD(), afterNumber)
							}
			    }
        // String literal
        case '"' =>
					val (stringlit, afterString) = rest span { case (car, _) =>
						isStringChar(car)
					}
					val (check, _) #:: tail = afterString
					if(check == EndOfFile || check == '\n') {
						ctx.reporter.fatal("Bad String")
						(BAD() setPos currentPos, tail)
					}
					val string: String = stringlit.map(_._1.toString).foldLeft("")((x, y) => x + y)
					(STRINGLIT(string) setPos currentPos, afterString.tail)
				case _ =>
					currentChar match {
						case '<' => if(nextChar == '=') useTwo(LESSEQUALS()) else useOne(LESSTHAN())
						case '&' => if(nextChar == '&') useTwo(AND()) else {
							ctx.reporter.fatal("Unkown character")
							useOne(BAD())
						}
						case '|' => if(nextChar == '|') useTwo(OR()) else {
							ctx.reporter.fatal("Unkown character")
							useOne(BAD())
						}
						case '=' =>
							if(nextChar == '=') useTwo(EQUALS()) else if(nextChar == '>') useTwo(RARROW()) else useOne(EQSIGN())
						case '+' => if(nextChar == '+') useTwo(CONCAT()) else useOne(PLUS())
						//case '\n' => readToken(rest) //skip
						case _ => useOne(currentChar match {
							case ';' => SEMICOLON()
							case '-' => MINUS()
							case '*' => TIMES()
							case '/' => DIV()
							case '%' => MOD()
							case '!' => BANG()
							case '{' => LBRACE()
							case '}' => RBRACE()
							case '(' => LPAREN()
							case ')' => RPAREN()
							case ',' => COMMA()
							case ':' => COLON()
							case '.' => DOT()
							case '_' => UNDERSCORE()
							case _ => {
								ctx.reporter.fatal("Unrecognized input")
								BAD()
							}
						})
					}


					  // TODO: Replace this catch-all by additional cases for other tokens
               // (You can look at Tokens.scala for an exhaustive list of tokens)
               // There should also be a case for all remaining (invalid) characters in the end
      }
    }

    // To lex a file, call nextToken() until it returns the empty Stream as "rest"
    def tokenStream(s: Stream[Input]): Stream[Token] = {
      if (s.isEmpty) Stream()
      else {
        val (token, rest) = nextToken(s)
        token #:: tokenStream(rest)
      }
    }

    tokenStream(inputStream)
  }

  // Lexing all input files means putting the tokens from each file one after the other
  def run(ctx: Context)(files: List[File]): Stream[Token] = {
    files.toStream flatMap lexFile(ctx)
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Stream[Token], Unit] {
  def run(ctx: Context)(tokens: Stream[Token]): Unit = {
    tokens.toList foreach { t => println(s"$t(${t.position.withoutFile})") }
  }
}
