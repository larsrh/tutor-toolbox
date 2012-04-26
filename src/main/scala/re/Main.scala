package edu.tum.cs.theo.re

import scalaz._
import Scalaz._

import edu.tum.cs.theo._
import Util._

object Main extends Module {

	@Operation(name = "re-equal")
	def reEqual(args: Seq[String]) {
		if (args.length != 2) {
			println("supply exactly two REs")
			println("example: (a*b*)* (a|b)*")
		}
		else {
			println("Checking whether " + args(0) + " is equivalent to " + args(1))
			println(parseAndCheckEquality(args(0), args(1)) match {
				case Success(Some((proof, which))) =>
					"Not equivalent. Proof: '" + proof.mkString + "' (accepted by " + (which ? "first" | "second") + ")"
				case Success(None) =>
					"Equivalent."
				case Failure(nel) =>
					"Parse error:\n" + nel.list.mkString("\n")
			})
		}
	}

}
