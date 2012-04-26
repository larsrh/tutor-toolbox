package edu.tum.cs.theo

import scalaz._
import Scalaz._

package object cfg {

	def probeEquality[NT, T](length: Int, cnf1: ChomskyNF[NT, T], cnf2: ChomskyNF[NT, T]): Option[(List[T], Boolean, Map[(Int, Int), Set[NT]])] = 
		Util.compare(Util.sequenceIter(cnf1.terminals ++ cnf2.terminals, length).toIterable) { (word: IndexedSeq[T], cnf: ChomskyNF[NT, T]) => 
			val table = cnf.cyk(word: _*)
			(table, table get ((0, word.length - 1)) map { _ contains (cnf.start) } getOrElse false)
		} (cnf1, cnf2) map {
			case (proof, seq) =>
				val table = if (seq.head._2) seq.head._1 else seq(1)._1
				(proof toList, seq.head._2, table)
		}
	
	def parseAndProbeEquality(length: Int, s1: String, s2: String): ValidationNEL[String, Option[(List[Char], Boolean, Map[(Int, Int), Set[Either[Symbol, Int]]])]] =
		(Parser.parseToChomskyNF(s1).liftFailNel |@| Parser.parseToChomskyNF(s2).liftFailNel)(probeEquality(length, _, _))

}
