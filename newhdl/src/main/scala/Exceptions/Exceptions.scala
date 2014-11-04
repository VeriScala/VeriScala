package NewHDL.Exceptions

class HDLException(stmt: String) extends Exception(stmt)

class NoSuchModuleException(modName: Symbol)
    extends HDLException(List("module", modName.name, "not found!").mkString(" "))

// compile and simulation
class WrongNumberOfArgumentsException(
  modName: Symbol, expected: Int, defacto: Int)
    extends HDLException(List(
      "wrong number of arguments for module ", modName.name,
      ", expecting ", expected.toString,
      ", get ", defacto.toString, ".").mkString(""))

class UndeclaredRegisterException(register: Symbol)
    extends HDLException("register %s is not declared!".format(register.name))

class NotEnoughBitsException(
  sigName: String, value: Int, expected: Int, defacto: Int)
    extends HDLException(
  "not enough bits to hold value %d in %s, require %d, get %d".format(
    value, sigName, expected, defacto))

class InvalidAssignmentException extends HDLException(
  "Can not assign signed signal to unsigned register!")

class BitwiseTypeMismatchException
    extends IllegalArgumentException(
  "Bitwise operations can only be applied to two signals with the same type!")
