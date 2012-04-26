package edu.tum.cs.theo

import scalaz._
import Scalaz._

package object re {

	implicit def EmptyPlusMonoid[EP[_], T](implicit e: Empty[EP], p: Plus[EP]) = new Monoid[EP[T]] {
		val zero = e.empty[T]
		def append(s1: EP[T], s2: => EP[T]) = s1 <+> s2
	}

	implicit def MyOptionPlus: Plus[Option] = new Plus[Option] {
		def plus[A](a1: Option[A], a2: => Option[A]) = {
			require(!a1.isDefined || !a2.isDefined)
			a1 orElse a2
		}
	}

	def renumberer[T]: T => Int = {
		var counter = 0
		var map: Map[T, Int] = Map()
		new (T => Int) { // TODO bork bork bork
			def apply(t: T) =
				map get t getOrElse {
					counter += 1
					map += (t -> counter)
					counter
				}
		}
	}

	def checkEquality[S, T, U](da1: DeterministicAutomaton[S, T], da2: DeterministicAutomaton[U, T]): Option[(List[T], Boolean)] = {
		(da1.combine(da2, DeterministicAutomaton.EqualCheck).nonEmpty: @unchecked) match {
			case None => None
			case Some((proof, Some(state))) => Some((proof, da1 isAccepting state._1))
		}
	}
		
	
	def checkEquality(s1: String, s2: String): Option[(List[Char], Boolean)] = checkEquality(Parser.parseToDFA(s1), Parser.parseToDFA(s2))

}
