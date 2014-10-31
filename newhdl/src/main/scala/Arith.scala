package NewHDL.Core

import NewHDL.Simulation.Core.BasicSimulations

trait Arith extends Base { this: ArithCompiler =>
  import HDLBase._

  implicit def hdlarithable2ha(x: HDL[Arithable]) = _HA(x)

  case class HDLAdd[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLSub[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLMul[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLDiv[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class _HA(nature: HDL[Arithable]) {
    def +(another: _HA) = HDLAdd(nature, another.nature)
    def -(another: _HA) = HDLSub(nature, another.nature)
    def *(another: _HA) = HDLMul(nature, another.nature)
    def /(another: _HA) = HDLDiv(nature, another.nature)
  }

  override protected def getSenslist(exp: HDLExp[Any]): Seq[HDLReg[Any]] =
    exp match {
      case HDLRev(x) => getSenslist(x)
      case HDLAdd(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLSub(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLMul(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLDiv(x, y) => getSenslist(x) ++ getSenslist(y)
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

trait ArithSimulations extends BasicSimulations { this: Arith =>
  import HDLBase._

  override def exec[T](exp: HDLExp[T]): List[Int] = {
    exp match {
      case HDLRev(x) =>
        List(if (exec(x)(0) > 0) 0 else 1)
      case HDLAdd(x, y) =>
        val p = exec(x).zip(exec(y))
        p.flatMap(tuple => exec(tuple._1 + tuple._2))
      case _ => super.exec(exp)
    }
  }
}
