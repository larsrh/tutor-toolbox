package edu.tum.cs.theo

object Main {

	val modules = List(
		re.Main,
		cfg.Main
	)

	def main(args: Array[String]) = {
		if (args.length < 1) {
			println("Available operations:")
			Module.availableOperations foreach { op =>
				println("* " + op)
			}
		}
		else {
			Module.invokeOperation(args(0), args.tail)
		}
	}

}
