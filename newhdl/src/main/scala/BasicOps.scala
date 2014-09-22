package NewHDL.Core

import scala.reflect.macros.Context
import scala.language.experimental.macros

object HDLBase {

  def hdlval(x: Any): Int = x match {
    case Signed(s, _) => s
    case Unsigned(u, _) => u
    case Bool(b) => b
    case b: Boolean => if (b) 1 else 0
    case i: Int => i
  }

  trait Arithable
  class HDLPrimitive(val length: Int)

  case class Bool(val value: Int)
      extends HDLPrimitive(1) with Arithable
  case class Signed(val value: Int, override val length: Int)
      extends HDLPrimitive(length) with Arithable
  case class Unsigned(val value: Int, override val length: Int)
      extends HDLPrimitive(length) with Arithable

  implicit def bool2hdlboolreg(x: Boolean) = new HDLReg[Boolean](x)
  implicit def int2hdlboolreg(x: Int) = new HDLReg[Boolean](
    if (x != 0) true else false)
  implicit def signed2hdlsigned(x: Signed) = new HDLReg[Signed](x)
  implicit def unsigned2hdlunsigned(x: Unsigned) = new HDLReg[Unsigned](x)

  abstract class HDLExp[+T] {
    def is[S >: T](other: HDLExp[S]) = HDLEquals[S](this, other)
  }

  case class HDLAssign[T](lhs: HDLReg[T], rhs: HDLExp[T]) extends HDLExp[T]

  case class HDLEquals[T](lhs: HDLExp[T], rhs: HDLExp[T]) extends HDLExp[Boolean]

  case class HDLWhen[T](cond: HDLExp[Boolean],
    suc: Seq[HDLExp[T]], fal: Seq[HDLExp[T]])
      extends HDLExp[T]

  abstract class HDLDef[+T] extends HDLExp[T]

  class HDLReg[+T](_value: T) extends HDLDef[T] {

    var name: Option[String] = None
    var out: Boolean = false
    var reg: Boolean = true

    def setName(_name: String) {
      name = Some(_name)
    }
    def getName = name match {
      case Some(n) => n
      case None => value.toString
    }
    def isConst = name == None
    def value = hdlval(_value)

    def :=[S >: T](rhs: HDLExp[S]) = {
      out = true
      HDLAssign(this, rhs)
    }

    def length = _value match {
      case p: HDLPrimitive => p.length
      case b: Boolean => 1
    }

    def lengthString =
      if (length > 1) "[" + (length - 1) + ":0] "
      else ""
  }

  case class HDLConst[T](unit: T) extends HDLDef[T]

  abstract class HDLBlock(val exps: Seq[HDLExp[Any]])

  case class HDLSyncBlock(val reg: HDLReg[Boolean], val when: Int,
    override val exps: Seq[HDLExp[Any]])
      extends HDLBlock(exps)

  case class HDLAsyncBlock(val senslist: Seq[HDLReg[Any]],
    override val exps: Seq[HDLExp[Any]])
      extends HDLBlock(exps)

  class HDLModule(_name: String,
    _params: List[HDLReg[Any]], _blocks: List[HDLBlock]) {
    val name = _name
    val blocks = _blocks
    val params = _params
    var analyzed = false
  }

  def moduleImpl(c: Context)(blocks: c.Expr[HDLBlock]*):
      c.Expr[HDLModule] = {
    import c.universe._

    // replace "enclosingMethod" with "enclosingDef"
    // in the future version of Scala!
    c.enclosingMethod match {
      case DefDef(_, moduleName, _, params, _, _) =>
        val names = params(0).map((param) => param match {
          case ValDef(_, name, _, _) => name
        })
        // set name for parameters
        val l = names.map((name) =>
          Apply(Select(
            Ident(newTermName(name.toString)), newTermName("setName")),
            List(Literal(Constant(name.toString)))))
        // construct the HDLModule
        val r = Apply(Select(New(Ident(newTypeName("HDLModule"))),
          nme.CONSTRUCTOR),
          List(Literal(Constant(moduleName.decoded)),
            Apply(Select(Ident("List"), newTermName("apply")),
              names.map(Ident(_)).toList),
            Apply(Select(Ident("List"), newTermName("apply")),
              blocks.map(_.tree).toList)))
        // return above two as a block
        c.Expr[HDLModule](Block(l, r))
      case _ =>
        c.Expr[HDLModule](
          Apply(Select(New(Ident(newTypeName("HDLModule"))),
            nme.CONSTRUCTOR),
            List(Literal(Constant("")), Literal(Constant(null)))))
    }
  }
}

trait Base {
  import HDLBase._

  type HDL[T] = HDLReg[T]

  def module(blocks: HDLBlock*): HDLModule = macro moduleImpl

  protected def getSenslist(exp: HDLExp[Any]): Seq[HDLReg[Any]] = exp match {
    case HDLWhen(cond, suc, fal) =>
      getSenslist(cond) ++ getSenslist(suc) ++ getSenslist(fal)
    case HDLAssign(_, rhs) => getSenslist(rhs)
    case r: HDLReg[Any] => if (!r.isConst) Seq(r) else Seq()
  }

  private def getSenslist(exps: Seq[HDLExp[Any]]): Seq[HDLReg[Any]] = {
    (for (exp <- exps) yield getSenslist(exp)).flatten
  }

  def sync(clk: HDLReg[Boolean], when: Int)(exps: HDLExp[Any]*) =
    HDLSyncBlock(clk, when, exps)

  def async(exps: HDLExp[Any]*) = {
    val senslist = getSenslist(exps)
    HDLAsyncBlock(senslist, exps)
  }

  case class when[T](cond: HDLExp[Boolean])(exps: HDLExp[T]*) {
    def otherwise(otherexps: HDLExp[T]*) = HDLWhen[T](cond, exps, otherexps)
  }
}

trait BasicOps extends Base { this: Compiler =>
}
trait Compiler extends Base {
  import HDLBase._

  def compile[T](exp: HDLExp[T]): String = exp match {
    case HDLWhen(cond, suc, fal) =>
      val c = cond match {
        case cr: HDLReg[T] => compile(cr) + " == 1"
        case _ => compile(cond)
      }
      "if (" + c + ") begin\n" + suc.map(compile(_)).mkString(";\n") +
      "\nend\nelse begin\n" + fal.map(compile(_)).mkString(";\n") + "\nend\n"
    case HDLAssign(lhs, rhs) =>
      compile(lhs) + " <= " + compile(rhs) + ";"
    case r: HDLReg[T] => r.getName
  }

  def compile(b: HDLBlock): String = {
    val stmts = (for (exp <- b.exps) yield compile(exp)).mkString("\n")
    b match {
      case HDLSyncBlock(reg, when, _) =>
        "always @(" + (if (when == 1) "posedge " else "negedge ") + reg.getName +
        ") begin\n" + stmts +
        "end\n"
      case HDLAsyncBlock(senslist, _) =>
        "always @(" + senslist.map(compile(_)).mkString(", ") +
        ") begin\n" + stmts + "end\n"
    }
  }

  def compile(m: HDLModule): String = {
    val paramNames = m.params.map(_.getName)
    val regs = m.params.filter(_.out).filter(_.reg)
    "module " + m.name + "(\n" +
    paramNames.mkString(",\n") + "\n);\n\n" +
    m.params.map((p) =>
      (if (p.out) "output " else "input ")
        + p.lengthString + p.getName + ";\n").toList.sorted.mkString("") +
    regs.map(p => "reg " +
      p.lengthString +
      p.getName + ";\n").toList.sorted.mkString("") +
    "\ninitial begin\n" + regs.map((p) =>
      p.getName + " = " + p.value + ";\n").mkString("") + "end\n\n" +
    (for (block <- m.blocks) yield compile(block)).mkString("\n") + "\nendmodule\n"
  }
}
