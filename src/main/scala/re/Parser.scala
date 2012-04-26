package edu.tum.cs.theo.re

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

import scalaz._

import edu.tum.cs.theo.Util._

object Parser {

	private object REParser extends CommonParsers {
		def S: Parser[RegEx[Char]] = (E ~ ("|" ~> S)) ^^ { case e ~ s => Alternative(e, s) } | E
		def E: Parser[RegEx[Char]] = (M ~ E ^^ { case m ~ e => Sequence(m, e)}) | M
		def M: Parser[RegEx[Char]] = (T <~ "*" ^^ { Repetition(_) }) | (T <~ "?" ^^ { Opt(_) }) | T
		def T: Parser[RegEx[Char]] = ("(" ~> S <~ ")") | Sym
		def Sym: Parser[RegEx[Char]] = new Regex("[a-zA-Z0-9]") ^^ { case str if str.length == 1 => Symbol(str.charAt(0)) }
	}

	def parse(s: String): Validation[String, RegEx[Char]] = REParser.parseAll(_.S, s)

	def parseToDFA(s: String): Validation[String, DeterministicAutomaton[Int, Char]] =
		parse(s).map(_.toAutomaton.mapSyms(_.get).deterministic.renumberStates)

}
