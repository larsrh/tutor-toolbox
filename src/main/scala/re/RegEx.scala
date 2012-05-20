package edu.tum.cs.theo.re

object RegEx {

	type MRE[T] = RegEx[MarkedSymbol[T]]

	sealed abstract class State[+T]
	case class SymbolState[+T](sym: T) extends State[T]
	case object StartState extends State[Nothing]

}

sealed abstract class RegEx[T] {

	import RegEx._

	private def prepare: RegEx[Option[T]] = map { Some(_) }

	def getFollowMap =
		// this function essentially transforms a Set[Option[U],Option[U]] 
		// into a Map[T, Set[Option[U]]], thus grouping each pair together
		// where the first element is not None, so the Option 'wrapper' from
		// the first element is stripped away, but kept at the second element
		// This is done because we use 'None' to denote a specialized last
		// character which could possibly follow any character, but itself
		// has no followers.
		for ((k, v) <- (prepare.follow(Set(None)) groupBy { _._1 }) if k ne None)
			yield k.get -> (v map { _._2 })

	def toAutomaton = {
		type SMS = State[MarkedSymbol[T]]

		def makeTransition(from: SMS, to: MarkedSymbol[T])
			: ((SMS, MarkedSymbol[T]), Set[SMS])  =
			((from, to), Set(states(to)))

		// implementation of Berry-Sethi method
		// We first prepare ourselves via marking, then we create a state
		// for each marked symbol. Then we iterate over the follow map
		// and create the appropriate transitions. We will get a deterministic
		// automaton at last (based on the markings) which can be converted
		// further by the caller.
		lazy val markedRegex = mark
		lazy val states = Map(markedRegex.symbols map { sym => sym -> SymbolState(sym) } toList :_*)
		var transitions = Map.empty[(SMS, MarkedSymbol[T]), Set[SMS]]
		var endStates = Set.empty[SMS]

		for (first <- markedRegex.first)
			transitions += makeTransition(StartState, first)

		for ((sym, followMap) <- markedRegex.getFollowMap;
			 follow <- followMap)
			follow match {
				case Some(s) => transitions += makeTransition(states(sym), s)
				case None => endStates += states(sym)
			}

		if (markedRegex.containsEmptyWord)
			endStates += StartState

		new Automaton[SMS, MarkedSymbol[T]](Set(StartState), transitions, endStates)
	}

	def mark: MRE[T] = {
		val marking = new Marking[T]
		map { marking.newSymbol(_) }
	}

	def map[U](f: T => U): RegEx[U]
	def containsEmptyWord: Boolean
	def first: Set[T]
	def follow(syms: Set[T]): Set[(T, T)]
	def symbols: Set[T]
}

final case class Symbol[T](sym: T) extends RegEx[T] {
	override def map[U](f: T => U) = Symbol[U](f(sym))
	override def first = Set(sym)
	override def containsEmptyWord = false
	override def follow(syms: Set[T]) = syms map { sym -> _ }
	override def symbols = Set(sym)
}

final case class Sequence[T](exprs: (RegEx[T], RegEx[T])) extends RegEx[T] {
	override def map[U](f: T => U) = Sequence[U](exprs._1 map f, exprs._2 map f)
	override def containsEmptyWord = exprs._1.containsEmptyWord && exprs._2.containsEmptyWord
	override def symbols = exprs._1.symbols ++ exprs._2.symbols

	override def first =
		if (exprs._1.containsEmptyWord)
			exprs._1.first ++ exprs._2.first
		else
			exprs._1.first

	override def follow(syms: Set[T]) =
		(exprs._2 follow syms) ++
		(exprs._1 follow (
			if (exprs._2.containsEmptyWord)
				exprs._2.first ++ syms
			else
				exprs._2.first
		))
}

final case class Repetition[T](expr: RegEx[T]) extends RegEx[T] {
	override def map[U](f: T => U) = Repetition[U](expr map f)
	override def first = expr.first
	override def containsEmptyWord = true
	override def follow(syms: Set[T]) = expr follow (expr.first ++ syms)
	override def symbols = expr.symbols
}

final case class Alternative[T](exprs: (RegEx[T], RegEx[T])) extends RegEx[T] {
	override def map[U](f: T => U) = Alternative[U](exprs._1 map f, exprs._2 map f)
	override def first = exprs._1.first ++ exprs._2.first
	override def containsEmptyWord = exprs._1.containsEmptyWord || exprs._2.containsEmptyWord
	override def follow(syms: Set[T]) = (exprs._1 follow syms) ++ (exprs._2 follow syms)
	override def symbols = exprs._1.symbols ++ exprs._2.symbols
}

final case class Opt[T](expr: RegEx[T]) extends RegEx[T] {
	override def map[U](f: T => U) = Opt[U](expr map f)
	override def containsEmptyWord = true
	override def first = expr.first
	override def follow(syms: Set[T]) = expr follow syms
	override def symbols = expr.symbols
}

final case class Epsilon[T]() extends RegEx[T] {
	override def map[U](f: T => U) = Epsilon[U]()
	override def containsEmptyWord = true
	override def first = Set()
	override def follow(syms: Set[T]) = Set()
	override def symbols = Set()
}
