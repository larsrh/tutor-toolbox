
package theo

object Main {

	def main(args: Array[String]) {
		if (args.length != 2) {
			println("supply exactly two REs")
			println("example: (a*b*)* (a|b)*")
		}
		else {
			println("Checking whether " + args(0) + " is equivalent to " + args(1))
			println(DeterministicAutomaton.checkEquality(args(0), args(1)) map { 
				"Not equivalent. Proof: " + _.mkString
			} getOrElse {
				"Equivalent."
			})
		}
	}

}
