/** This module contains basic functionality for Amy,
  * including stub implementations for some built-in functions
  * (implemented in WASM or JavaScript)
  */
object Std {
  def printInt(i: Int): Unit = {
    error(intToString(i)) // Stub implementation
  }
  def printString(s: String): Unit = {
    error(s) // Stub implementation
  }
  def printBoolean(b: Boolean): Unit = {
    printString(booleanToString(b))
  }

  def readString(): String = {
    error("") // Stub implementation
  }

  def readInt(): Int = {
    error("") // Stub implementation
  }

  def intToString(i: Int): String = {
    if (i < 0) {
      "-" ++ intToString(-i)
    } else {
      val rem: Int = i % 10;
      val div: Int = i / 10;
      if (div == 0) { digitToString(rem) }
      else { intToString(div) ++ digitToString(rem) }
    }
  }
  def digitToString(i: Int): String = {
		if(i < 5) {
			if(i < 2) {
				if(i == 0) {"0"}
				else {"1"}
			}
			else {
				if(i == 3) {"3"}
				else {"4"}
			}
		}
		else {
			if(i < 7) {
				if(i == 5) {"5"}
				else {"6"}
			}
			else {
				if(i == 7) {"7"}
				else {
					if(i == 8) {"8"}
					else {"9"}
				}
			}
		}
    //error("") // Stub implementation
  }
  def booleanToString(b: Boolean): String = {
    if (b) { "true" } else { "false" }
  }
}
