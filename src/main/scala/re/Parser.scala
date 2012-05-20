package edu.tum.cs.theo.re

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

import scalaz._

import edu.tum.cs.theo.Util._

object Parser {

	private object REParser extends CommonParsers {
		def S: Parser[RegEx[Char]] = E ~ opt(
			"|" ~> S ^^ { s => (e: RegEx[Char]) => Alternative(e, s) }
		) ^^ { case e ~ f => f map (_(e)) getOrElse e }
		def E: Parser[RegEx[Char]] = M ~ opt(
			E ^^ { e => (m: RegEx[Char]) => Sequence(m, e)}
		) ^^ { case m ~ f => f map (_(m)) getOrElse m }
		def M: Parser[RegEx[Char]] = T ~ opt(
			("*" ^^^ { (r: RegEx[Char]) => Repetition(r) }) |
			("?" ^^^ { (r: RegEx[Char]) => Opt(r) })
		) ^^ { case p ~ f => f map (_(p)) getOrElse p }
		def T: Parser[RegEx[Char]] = ("(" ~> S <~ ")") | ("ε" ^^^ Epsilon[Char]()) | Sym
		def Sym: Parser[RegEx[Char]] = new Regex("[a-zA-Z0-9]") ^^ { case str if str.length == 1 => Symbol(str.charAt(0)) }
	}

	def parse(s: String): Validation[String, RegEx[Char]] = REParser.parseAll(_.S, s)

	def parseToDFA(s: String): Validation[String, DeterministicAutomaton[Int, Char]] =
		parse(s).map(_.toAutomaton.mapSyms(_.get).deterministic.renumberStates)

}
