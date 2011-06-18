
package theo

object Module {

	private var operations = Map[String, (Seq[String] => Unit)]()

	def availableOperations = operations.keySet
	
	def invokeOperation(op: String, args: Seq[String]) = operations(op)(args)

}

abstract class Module { self: Singleton =>

	getClass.getDeclaredMethods.foreach { m =>
		Option(m.getAnnotation(classOf[Operation])) match {
			case Some(op) => Module.operations += op.name() -> { args: Seq[String] =>
				m.invoke(self, args)
			}
			case None =>
		}
	}

}

