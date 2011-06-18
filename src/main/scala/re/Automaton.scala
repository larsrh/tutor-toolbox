
package theo.re

import scala.collection._

import scalaz._
import Scalaz._

abstract class AutomatonLike[S, T, M[_] : Monad : Plus : Empty] {

	type Mapped[U, V] <: AutomatonLike[U, V, M]

	def start: M[S]
	def transition: Map[(S, T), M[S]]
	def end: Set[S]

	lazy val syms = transition.keySet.map(_._2)

	protected[this] def withParams[U, V](newStart: M[U], newTransition: Map[(U, V), M[U]], newEnd: Set[U]): Mapped[U, V]
	protected[this] def isAccepting(metaState: M[S]): Boolean

	final def step(metaState: M[S], sym: T): M[S] =	// w00t scalaz
		metaState flatMap { s => 
			transition get ((s, sym)) getOrElse (implicitly[Empty[M]].empty[S])
		}

	final def eval(word: T*): Boolean = isAccepting((start /: word)(step))

	final def mapSyms[V](f: T => V): Mapped[S, V] = {
		val newTrans: Map[(S, V), M[S]] = transition groupBy { case ((state, sym), _) => (state, f(sym)) } mapValues { _.values.sumr }
		withParams(start, newTrans, end)
	}
	
	final def mapStates[U](f: S => U): Mapped[U, T] = {
		val newTrans: Map[(U, T), M[U]] = transition groupBy { case ((state, sym), _) => (f(state), sym) } mapValues { _.values.sumr.map(f) }
		withParams(start map f, newTrans, end map f)
	}

	final def renumberStates: Mapped[Int, T] = mapStates(renumberer)

	final def nonEmpty: Option[List[T]] = {
		var queue = mutable.Queue((start, List.empty[T]))
		var done = Set[M[S]]()

		while (!queue.isEmpty) {
			val (metaState, proof) = queue.dequeue()

			if (!(done contains metaState)) {
				if (isAccepting(metaState))
					return Some(proof.reverse)

				for (sym <- syms) {
					val stepped = step(metaState, sym)
					val newProof = sym :: proof
					queue.enqueue((stepped, newProof))
				}
				done += metaState
			}
		}

		None
	}

}

class Automaton[S, T](override val start: Set[S], override val transition: Map[(S, T), Set[S]], override val end: Set[S]) extends AutomatonLike[S, T, Set] {

	override type Mapped[U, V] = Automaton[U, V]

	override protected[this] def isAccepting(states: Set[S]) = !(states intersect end).isEmpty

	override protected[this] def withParams[U, V](newStart: Set[U], newTransition: Map[(U, V), Set[U]], newEnd: Set[U]) = new Automaton(newStart, newTransition, newEnd)

	def deterministic: DeterministicAutomaton[Set[S], T] = {
		var queue = mutable.Queue(start)
		var done = Set[Set[S]]()
		var newTransition = Map[(Set[S], T), Option[Set[S]]]()

		while (!queue.isEmpty) {
			val metaState = queue.dequeue()
			if (!(done contains metaState)) {
				for (sym <- syms) {
					val stepped = step(metaState, sym)
					queue.enqueue(stepped)
					newTransition += (metaState, sym) -> Some(stepped)
				}
				done += metaState
			}
		}

		new DeterministicAutomaton(start, newTransition, done filter { d => !(d intersect end).isEmpty })
	}

}

object DeterministicAutomaton {

	def empty[S, T] = new DeterministicAutomaton[S, T](Option.empty[S], Map[(S, T), Option[S]](), Set[S]())

	sealed trait CombineMode 
	case object Intersection extends CombineMode 
	case object Union extends CombineMode
	case object EqualCheck extends CombineMode // yields an automaton which has a non-empty language iff the original automata are not equivalent

	def checkEquality[S, T, U](da1: DeterministicAutomaton[S, T], da2: DeterministicAutomaton[U, T]): Option[List[T]] =
		da1.combine(da2, EqualCheck).nonEmpty
	
	def checkEquality(s1: String, s2: String): Option[List[Char]] = checkEquality(Parser.parseToDFA(s1), Parser.parseToDFA(s2))

}

class DeterministicAutomaton[S, T] private(override val start: Option[S], override val transition: Map[(S, T), Option[S]], override val end: Set[S]) extends AutomatonLike[S, T, Option] {

	import DeterministicAutomaton._

	def this(start: S, transition: Map[(S, T), Option[S]], end: Set[S]) = this(Some(start), transition, end)

	override type Mapped[U, V] = DeterministicAutomaton[U, V]

	override protected[this] def isAccepting(state: Option[S]) = state exists { s => end contains s }

	override protected[this] def withParams[U, V](newStart: Option[U], newTransition: Map[(U, V), Option[U]], newEnd: Set[U]) = newStart map { s =>
		new DeterministicAutomaton(s, newTransition, newEnd)
	} getOrElse
		empty[U, V]
	
	def states: Set[S] = transition.keySet.map(_._1) ++ transition.values.flatten ++ end ++ start

	def combine[U](other: DeterministicAutomaton[U, T], mode: CombineMode): Mapped[(Option[S], Option[U]), T] = {
		type UU = (Option[S], Option[U])
		if (this.start.isEmpty || other.start.isEmpty) {
			empty[UU, T]
		}
		else {
			var queue = mutable.Queue((this.start, other.start))
			var done = Set[UU]()
			var newTransition = Map[(UU, T), Option[UU]]()
			val syms = this.syms ++ other.syms

			while (!queue.isEmpty) {
				val state = queue.dequeue()
				if (!(done contains state)) {
					val (from1, from2) = state
					for (sym <- syms) {
						val to = (this.step(from1, sym), other.step(from2, sym))
						queue.enqueue(to)
						newTransition += ((from1, from2), sym) -> Some(to)
					}
					done += state
				}
			}

			val newEnd: Set[UU] = mode match {
				case Union =>
					val e1: Set[UU] = for (e <- this.end; s <- other.states) yield (Some(e),Some(s))
					val e2: Set[UU] = for (s <- this.states; e <- other.end) yield (Some(s),Some(e))
					e1 union e2 union (this.end map { e => (Some(e), None) }) union (other.end map { e => (None, Some(e)) })
				case Intersection =>
					for (e1 <- this.end; e2 <- other.end) yield (Some(e1), Some(e2))
				case EqualCheck =>
					val e1: Set[UU] = for (e <- this.end; s <- other.states -- other.end) yield (Some(e),Some(s))
					val e2: Set[UU] = for (s <- this.states -- this.end;  e <- other.end) yield (Some(s),Some(e))
					e1 union e2 union (this.end map { e => (Some(e), None) }) union (other.end map { e => (None, Some(e)) })
			}

			new DeterministicAutomaton[UU, T]((this.start, other.start), newTransition, newEnd)
		}
	}

}
