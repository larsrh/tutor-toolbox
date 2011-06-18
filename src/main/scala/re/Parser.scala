
package theo.re

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

object Parser {

	private object REParser extends RegexParsers {
		def S: Parser[RegEx[Char]] = (E ~ ("|" ~> S)) ^^ { case e ~ s => Alternative(e, s) } | E
		def E: Parser[RegEx[Char]] = (M ~ E ^^ { case m ~ e => Sequence(m, e)}) | M
		def M: Parser[RegEx[Char]] = (T <~ "*" ^^ { Repetition(_) }) | (T <~ "?" ^^ { Opt(_) }) | T
		def T: Parser[RegEx[Char]] = ("(" ~> S <~ ")") | Sym
		def Sym: Parser[RegEx[Char]] = new Regex("[a-zA-Z0-9]") ^^ { case str if str.length == 1 => Symbol(str.charAt(0)) }
	}

	def parse(s: String): RegEx[Char] = REParser.parseAll(REParser.S, s).get 
	def parseToDFA(s: String): DeterministicAutomaton[Int, Char] = parse(s).toAutomaton.mapSyms(_.get).deterministic.renumberStates

}