package edu.tum.cs.theo.cfg

import scalaz._
import Scalaz._

import edu.tum.cs.theo._
import Util._

object Main extends Module {

	@Operation(name = "cfg-probe-equal")
	def reEqual(args: Seq[String]) {
		if (args.length != 3) {
			println("supply a maximum word length and exactly two CFGs")
			println("example: 10 S:S->ab S:S->a|b")
		}
		else {
			println("Probing whether " + args(1) + " is equivalent to " + args(2))
			println(parseAndProbeEquality(args(0).toInt, args(1), args(2)) match {
				case Success(Some((proof, which, table))) =>
					"Not equivalent. Proof: '" + proof.mkString + "' (accepted by " + (which ? "first" | "second") + ")\n" +
					"Accepting table: \n" + table.mkString("\n")
				case Success(None) =>
					"Maybe equivalent."
				case Failure(nel) =>
					"Parse error:\n" + nel.list.mkString("\n")
			})
		}
	}

}
