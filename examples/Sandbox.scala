object Sandbox {
	  abstract class Option
	  case class None() extends Option
		case class Some(v: Int) extends Option
		//case class Dumb() extends Op
		val x: Option = None(); x
}
