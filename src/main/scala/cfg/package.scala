
package theo

import theo._

package object cfg {

	def probeEquality[NT1, NT2, T](length: Int, cnf1: ChomskyNF[NT1, T], cnf2: ChomskyNF[NT2, T]): Option[(List[T], Boolean)] = 
		Util.compare(Util.sequenceIter(cnf1.terminals ++ cnf2.terminals, length).toIterable) { (word: IndexedSeq[T], cnf: ChomskyNF[_, T]) => 
			cnf.contains(word: _*)
		} (cnf1, cnf2) map {
			case (proof, seq) => (proof toList, seq head)
		}
	
	def probeEquality(length: Int, s1: String, s2: String): Option[(List[Char], Boolean)] = probeEquality(length, Parser.parseToChomskyNF(s1), Parser.parseToChomskyNF(s2))

}

