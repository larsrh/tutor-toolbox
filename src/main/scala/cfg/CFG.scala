
package theo.cfg

import collection.mutable

trait CFG[NT, +T] {
	val start: NT
}

case class GenericCFG[NT, T](
	override val start: NT,
	productions: Set[(NT, List[Either[NT, T]])]
) extends CFG[NT, T] {

	type Productions = Set[(NT, List[Either[NT, T]])]

	def toChomskyNF: ChomskyNF[Either[NT, Int], T] = {
		type NewProductions = Set[(Either[NT, Int], List[Either[Either[NT, Int], T]])]
		type ChomskyProductions = Set[(Either[NT, Int], Either[T, (Either[NT, Int], Either[NT, Int])])]

		var freshNT = 0

		// The following is not optimized. It should work, however.

		def eliminateEmpty(productions: Productions): Productions = {
			var (empty, nonEmpty) = productions partition { case (_, list) => list == Nil }
			var dirty = true
		
			while (dirty) {
				var addEmpty, addNonEmpty: Productions = Set()
				for ((lhsE, _) <- empty; (lhsNE, rhs) <- nonEmpty) {
					rhs indexOf Left(lhsE) match {
						case -1 =>
						case n =>
							rhs.patch(n, Nil, 1) match {
								case Nil => addEmpty += ((lhsNE, Nil))
								case list => addNonEmpty += ((lhsNE, list))
							}
					}
				}

				val (eSize, neSize) = (empty.size, nonEmpty.size)
				empty ++= addEmpty
				nonEmpty ++= addNonEmpty
				dirty = empty.size > eSize || nonEmpty.size > neSize
			}

			if (empty contains (start, Nil)) nonEmpty += ((start, Nil))
			nonEmpty
		}

		def eliminateChain(productions: Productions): Productions = {
			var (chain, nonChain) = productions partition { case (_, List(Left(_))) => true; case _ => false }
			var dirty = true

			while (dirty) {
				var addChain, addNonChain: Productions = Set()
				for ((nt1, List(Left(nt2))) <- chain; (`nt2`, rhs) <- nonChain) {
					rhs match {
						case List(Left(_)) => addChain += ((nt1, rhs))
						case _ => addNonChain += ((nt1, rhs))
					}
				}

				val (cSize, ncSize) = (chain.size, nonChain.size)
				nonChain ++= addNonChain
				chain ++= addChain
				dirty = nonChain.size > ncSize || chain.size > cSize
			}

			nonChain 
		}

		def convertToNew(productions: Productions): NewProductions = {
			productions map { case (nt, rhs) =>
				(Left(nt), rhs map {
					case Left(n) => Left(Left(n))
					case Right(t) => Right(t)
				})
			}
		}

		def addTerminalNTs(productions: NewProductions): NewProductions = {
			var added = Map[T, Int]()
			for ((_, rhs) <- productions if rhs.length >= 2;
			     Right(term) <- rhs) {
				added get term getOrElse {
					added += term -> freshNT
					freshNT += 1
				}
			}

			val mapped = for ((nt, rhs) <- productions) yield {
				if (rhs.length < 2) {
					(nt, rhs)
				}
				else {
					(nt, rhs map {
						case l @ Left(_) => l
						case r @ Right(term) => Left(Right(added(term)))
					})
				}
			}
			
			mapped ++ (added map { case (term, index) =>
				(Right(index), List(Right(term)))
			})
		}

		def split(productions: NewProductions): (ChomskyProductions, Boolean) = {
			freshNT += 1
			val cp = productions flatMap { case (nt, rhs) => (rhs length match {

				case 0 => // Epsilon
					assert(nt == Left(start))
					Nil

				case 1 => // T
					rhs match {
						case List(Right(t)) => List((nt, Left(t)))
						case _ => assert(false); Nil
					}

				case 2 => // NT, NT
					rhs match {
						case List(Left(nt1), Left(nt2)) => List((nt, Right((nt1, nt2))))
						case _ => assert(false); Nil
					}

				case _ => // NT*
					val seq: IndexedSeq[Either[NT, Int]] = rhs map {
						case Left(nt) => nt
					} toIndexedSeq

					val fresh = freshNT
					val n = seq.length
					val split = (1 to n - 3) map { i =>
						val value = freshNT
						val p = Right(value) -> Right((seq(i), Right(value + 1)))
						freshNT += 1
						p
					}

					val value = freshNT
					freshNT += 1

					(nt -> Right((seq(0), Right(fresh)))) :: (Right(value) -> Right((seq(n-2), seq(n-1)))) :: (split toList)

			}): List[(Either[NT, Int], Either[T, (Either[NT, Int], Either[NT, Int])])] }
			(cp, productions contains ((Left(start), Nil)))
		}

		val nonEmpty = eliminateEmpty(productions)
//		println(nonEmpty mkString "\n")
//		println("***")
		val nonChain = eliminateChain(nonEmpty)
//		println(nonChain mkString "\n")
//		println("***")
		val converted = convertToNew(nonChain)
//		println(converted mkString "\n")
//		println("***")
		val added = addTerminalNTs(converted)
//		println(added mkString "\n")
//		println("***")
		val (splitted, containsEmpty) = split(added)
//		println("containsEmpty = " + containsEmpty)
//		println(splitted mkString "\n")

		ChomskyNF(Left(start), splitted, containsEmpty)
	}

	override def toString = {
		def ntToString(nt: NT) = nt match {
			case sym: Symbol => "<" + sym.name + ">"
			case _ => nt.toString
		}

		productions map { case (nt, rhs) =>
			val left = (if (nt == start) "* " else "  ") + ntToString(nt)
			val right = rhs map {
				case Left(sym) => ntToString(sym)
				case Right(term) => term
			} mkString " "
			left + " -> " + right
		} mkString "\n"
	}

}

case class ChomskyNF[NT, T](
	override val start: NT,
	productions: Set[(NT, Either[T, (NT, NT)])],
	containsEmpty: Boolean
) extends CFG[NT, T] {

	def contains(word: T*) = {
		if (word.length == 0) {
			containsEmpty
		}
		else { // CYK
			def producingT(t: T) =
				for ((nt, Left(`t`)) <- productions) yield nt
			def producingNT(nts: (NT, NT)) =
				for ((nt, Right(`nts`)) <- productions) yield nt
			def allProducingNT(ntSet1: Set[NT], ntSet2: Set[NT]) =
				for (nt1 <- ntSet1; nt2 <- ntSet2; producer <- producingNT((nt1, nt2))) yield producer

			val len = word.length
			val w = word.toIndexedSeq
			var v = Map[(Int, Int), Set[NT]]()
			for (i <- 0 until len)
				v += (i, i) -> producingT(w(i))

			for (diff <- 1 to len;
			     i <- 0 to len - diff - 1; j = i + diff) 
				v += (i, j) -> ((i until j) flatMap { k => allProducingNT(v(i, k), v(k + 1, j)) } toSet)
			
//			println(v mkString "\n")

			v(0, len - 1) contains start
		}
	}

	override def toString = {
		def ntToString(nt: NT) = nt match {
			case sym: Symbol => "<" + sym.name + ">"
			case _ => nt.toString
		}

		productions map { case (nt, rhs) =>
			val left = (if (nt == start) "* " else "  ") + ntToString(nt)
			val right = rhs match {
				case Left(term) => term
				case Right((nt1, nt2)) => ntToString(nt1) + " " + ntToString(nt2)
			}
			left + " -> " + right
		} mkString "\n"
	}

}
