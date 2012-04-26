package edu.tum.cs.theo.cfg

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

import scalaz._

import edu.tum.cs.theo.Util._

object Parser {

	private object CFGParser extends CommonParsers {
		def cfg: Parser[GenericCFG[Symbol, Char]] = ((nonTerminal <~ ":") ~ repsep(rule, ";")) ^^ { case start ~ rules => GenericCFG(start, rules.flatten.toSet) }
		def rule: Parser[List[(Symbol, List[Either[Symbol, Char]])]] = ((nonTerminal <~ "->") ~ rhs) ^^ { case lhs ~ rhs => rhs map { (lhs, _) } }
		def rhs: Parser[List[List[Either[Symbol, Char]]]] = repsep(simpleRHS, "|")
		def simpleRHS: Parser[List[Either[Symbol, Char]]] = (terminal | (nonTerminal ^^ { Left(_) }))*
		def terminal: Parser[Either[Symbol, Char]] = new Regex("[a-z0-9]") ^^ { str => Right(str.charAt(0)) }
		def nonTerminal: Parser[Symbol] = new Regex("<[a-zA-Z0-9_]*>") ^^ { str => Symbol(str.tail.init) }
	}

	def parse(s: String): Validation[String, GenericCFG[Symbol, Char]] = CFGParser.parseAll(_.cfg, s)

	def parseToChomskyNF(s: String): Validation[String, ChomskyNF[Either[Symbol, Int], Char]] = parse(s).map(_.toChomskyNF)

}
