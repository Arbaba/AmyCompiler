object Logic {
	//a.b(); error("?")
	//val x : Int = if(true && false) { -(1 + 2)  } else { 3 }; x
	//val x: Int = 3;
	/*def try(i: Int): String = {
		i match {
			case 1 => "1"
			case 2 => "2"
			//case x => try(x-1)
		}
	}
	error(try(1))*/
	abstract class Seq
	case class Array(capacity: Int) extends Seq
  /*def booleanToString(b: Boolean): String = {
    if (b) { "true" } else { "false" }
  }
	error(booleanToString(() == ()))*/
	//() == ()
	val x: Array = Array(3); x
}
