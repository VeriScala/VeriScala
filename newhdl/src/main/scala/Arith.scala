package NewHDL.Core

trait Arith extends Base { this: ArithCompiler =>
  import HDLBase._

  implicit def hdlarithable2ha(x: HDL[Arithable]) = _HA(x)

  case class HDLAdd[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class _HA(nature: HDL[Arithable]) {
    def +(another: _HA) = HDLAdd(nature, another.nature)
  }

  override protected def getSenslist(exp: HDLExp[Any]): Seq[HDLReg[Any]] =
    exp match {
      case HDLAdd(x, y) => getSenslist(x) ++ getSenslist(y)
      case _ => super.getSenslist(exp)
    }
}

trait ArithCompiler extends Compiler { this: Arith =>
  import HDLBase._

  override def compile[T](exp: HDLExp[T]): String = exp match {
    case HDLAdd(x, y) =>
      compile(x) + " + " + compile(y)
    case _ => super.compile(exp)
  }
}
