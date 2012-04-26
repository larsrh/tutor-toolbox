package edu.tum.cs.theo.re

import scala.collection.mutable

import scalaz._
import Scalaz._

abstract class AutomatonLike[S, T, M[_] : Monad : Plus : Empty] {

	type Mapped[U, V] <: AutomatonLike[U, V, M]

	final def emptyS = implicitly[Empty[M]].empty[S]

	def start: M[S]
	def transition: Map[(S, T), M[S]]
	def end: Set[S]

	lazy val syms = transition.keySet.map(_._2)

	protected[this] def withParams[U, V](newStart: M[U], newTransition: Map[(U, V), M[U]], newEnd: Set[U]): Mapped[U, V]
	
	def isAccepting(metaState: M[S]): Boolean

	final def singleStep(state: S, sym: T): M[S] = transition get ((state, sym)) getOrElse emptyS

	final def step(metaState: M[S], sym: T): M[S] = metaState >>= (singleStep(_, sym))

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

	final def nonEmpty: Option[(List[T], M[S])] = {
		var queue = mutable.Queue((start, List.empty[T]))
		var done = Set.empty[M[S]]

		while (!queue.isEmpty) {
			val (metaState, proof) = queue.dequeue()

			if (!(done contains metaState)) {
				if (isAccepting(metaState))
					return Some((proof.reverse), metaState)

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

	override protected[this] def withParams[U, V](newStart: Set[U], newTransition: Map[(U, V), Set[U]], newEnd: Set[U]) = new Automaton(newStart, newTransition, newEnd)

	override def isAccepting(states: Set[S]) = !(states intersect end).isEmpty

	def deterministic: DeterministicAutomaton[Set[S], T] = {
		var queue = mutable.Queue(start)
		var done = Set.empty[Set[S]]
		var newTransition = Map.empty[(Set[S], T), Option[Set[S]]]

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

	def empty[S, T] = new DeterministicAutomaton[S, T](none, Map.empty[(S, T), Option[S]], Set.empty[S])

	sealed trait CombineMode 
	case object Intersection extends CombineMode 
	case object Union extends CombineMode
	case object EqualCheck extends CombineMode // yields an automaton which has a non-empty language iff the original automata are not equivalent

}

class DeterministicAutomaton[S, T] private(override val start: Option[S], override val transition: Map[(S, T), Option[S]], override val end: Set[S]) extends AutomatonLike[S, T, Option] {

	import DeterministicAutomaton._

	def this(start: S, transition: Map[(S, T), Option[S]], end: Set[S]) = this(Some(start), transition, end)

	override type Mapped[U, V] = DeterministicAutomaton[U, V]

	override protected[this] def withParams[U, V](newStart: Option[U], newTransition: Map[(U, V), Option[U]], newEnd: Set[U]) =
		newStart map (new DeterministicAutomaton(_, newTransition, newEnd)) getOrElse empty[U, V]
	
	override def isAccepting(state: Option[S]) = state exists { s => end contains s }

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
						newTransition += ((from1, from2), sym) -> to.some
					}
					done += state
				}
			}

			def toSome(tuple: (S, U)) = ((s: S) => s.some) <-: tuple :-> ((u: U) => u.some)

			val newEnd: Set[UU] = mode match {
				case Union =>
					((this.end <|*|> other.states) ++ (this.states <|*|> other.end) map toSome) ++
					(this.end map { e => (e.some, none) }) ++
					(other.end map { e => (none, e.some) })
				case Intersection =>
					(this.end <|*|> other.end) map toSome
				case EqualCheck =>
					((this.end <|*|> (other.states -- other.end)) ++ ((this.states -- this.end) <|*|> other.end) map toSome) ++
					(this.end map { e => (e.some, none) }) ++
					(other.end map { e => (none, e.some) })
			}

			new DeterministicAutomaton[UU, T]((this.start, other.start), newTransition, newEnd)
		}
	}

}
