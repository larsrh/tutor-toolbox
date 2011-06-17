
package theo

class Marking[T] {
	
	private var counter: Int = 0

	def newSymbol(sym: T) = {
		counter += 1
		MarkedSymbol[T](sym, counter)
	}
	
}

case class MarkedSymbol[+T] protected[theo](sym: T, mark: Int) {

	def get = sym

}
