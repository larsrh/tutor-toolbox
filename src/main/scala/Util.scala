
package theo

object Util {

	def sequenceIter[T](elems: Set[T], maxLen: Int = 15): Iterator[IndexedSeq[T]] = new Iterator[IndexedSeq[T]] {
		require(elems.size > 0)

		val elemSeq = elems.toIndexedSeq
		val count = elemSeq.length
		var indexes: IndexedSeq[Int] = IndexedSeq()

		def hasNext = indexes.length <= maxLen

		def next = {
			val ret = indexes map elemSeq.apply
			if (indexes.length == 0)
				indexes = IndexedSeq(0)
			else if (indexes forall (_ == count - 1))
				indexes = IndexedSeq.fill(indexes.length+1)(0)
			else {
				var carry = true
				val buf = Array.fill[Int](indexes.length)(0)
				for (i <- 0 until indexes.length) {
					if (!carry) {
						buf(i) = indexes(i)
					}	
					else if (indexes(i) < count-1) {
						buf(i) = indexes(i) + 1
						carry = false
					}
					else {
						buf(i) = 0
					}
				}
				indexes = buf.toIndexedSeq
			}
			ret
		}
	}

/*
	def compare(strs: String*): Boolean = {
		require(strs.size >= 2)

		val parsed = strs map Parser.parse
		val syms: Set[Char] = parsed.flatMap((r: RegEx[Char]) => r.symbols)(breakOut)
		val testSeqs = sequenceIter(syms, 15)

		val automata = parsed map (_.toAutomaton.mapSyms(_.get).renumberStates)

		val failed = (testSeqs.toSeq.par) exists { seq =>
			val results = automata map (_.eval(seq: _*))
			val same = results.toSet.size == 1
			if (!same) {
				Main.synchronized {
					println("Comparision failed for " + seq.mkString)
					println("Results per regular expression: ")
					for ((k, v) <- strs zip results)
						println("  " + k + ": " + v)
					Console.flush()
				}
			}
			!same
		}

		!failed
	}*/


}

