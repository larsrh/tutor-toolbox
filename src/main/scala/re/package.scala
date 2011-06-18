
package theo

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

}

