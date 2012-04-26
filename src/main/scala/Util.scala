package edu.tum.cs.theo

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

	def compare[T, U](examples: Iterable[U])(f: (U, T) => Boolean)(args: T*): Option[(U, Seq[Boolean])] = {
		require(args.size >= 2)

		val pargs = args.par
		examples.view flatMap { u =>
			val results = pargs map { t => f(u, t) } seq;
			if (results.toSet.size > 1)
				Some((u, results))
			else
				None
		} headOption 
	}

	implicit def enrichBoolean(b: Boolean) = new {
		def choose[T](ifTrue: => T, ifFalse: => T) =
			if (b) ifTrue else ifFalse
	}

}
