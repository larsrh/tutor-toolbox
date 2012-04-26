package edu.tum.cs.theo

import util.parsing.combinator.RegexParsers
import util.matching.Regex

import scalaz._
import Scalaz._

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

	def compare[T, U, I](examples: Iterable[U])(f: (U, T) => (I, Boolean))(args: T*): Option[(U, Seq[(I, Boolean)])] = {
		require(args.size >= 2)

		val pargs = args.par
		examples.view flatMap { u =>
			val results = pargs map { t => f(u, t) } seq;
			if (results.map(_._2).toSet.size > 1)
				Some((u, results))
			else
				None
		} headOption 
	}

	trait CommonParsers extends RegexParsers {

		final def parseAll[T](parser: this.type => Parser[T], input: String): Validation[String, T] = parseAll(parser(this), input) match {
			case NoSuccess(msg, _) => msg.fail
			case Success(result, _) => result.success
		}

	}


}
