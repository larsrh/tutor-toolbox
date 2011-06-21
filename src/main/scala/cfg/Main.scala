
package theo.cfg

import theo._

object Main extends Module {

	import Util._

	@Operation(name = "cfg-probe-equal")
	def reEqual(args: Seq[String]) {
		if (args.length != 3) {
			println("supply a maximum word length and exactly two CFGs")
			println("example: 10 <S>:<S>->ab <S>:<S>->a|b")
		}
		else {
			println("Probing whether " + args(1) + " is equivalent to " + args(2))
			println(probeEquality(args(0).toInt, args(1), args(2)) map { case (proof, which) =>
				"Not equivalent. Proof: '" + proof.mkString + "' (accepted by " + which.choose("first","second") + ")"
			} getOrElse {
				"Equivalent."
			})
		}
	}

}
