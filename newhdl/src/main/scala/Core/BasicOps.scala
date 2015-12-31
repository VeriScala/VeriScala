package NewHDL.Core

import scala.reflect.macros.blackbox
import scala.language.experimental.macros
import scala.annotation.tailrec
import scala.collection.mutable.Stack
import scala.util.DynamicVariable
import scala.math.pow
import java.net.{DatagramPacket, DatagramSocket, InetAddress}
import java.io.{File, PrintWriter}
import com.typesafe.config._

import NewHDL.Exceptions.NotEnoughBitsException
import NewHDL.Simulation.Core.Waiter

object HDLBase {

  val b0 = false
  val b1 = true

  def u(a: Int) = Unsigned(a, HDLPrimitive.getUnsignedSize(a))

  type HDL[T] = HDLReg[T]
  def HDL[T](x: T) = new HDLReg[T](x)
  type CHDL[T] = HDLCTypeReg[T]
  def CHDL[T](x: T) = new HDLCTypeReg[T](x)

  def hdlval(x: Any): Int = x match {
    case Signed(s, _) => s
    case Unsigned(u, _) => u
    case Bool(b) => b
    case b: Boolean => if (b) 1 else 0
    case i: Int => i
  }

  val currentMod = new DynamicVariable[HDLModule](null)

//  def HDLlize[T](x: Seq[T]): HDLList[T] = currentMod.value.HDLlize(x)
  def HDLlize[T](x: T): HDLReg[T] = currentMod.value.HDLlize(x)

  case class HDLRev[T](a: HDLExp[T]) extends HDLExp[T]

  class Register(val name: String, _value: Int,
    val length: Int, val signed: Boolean) {
    var value = _value
    protected var next: Int = value

    private var eventWaiters: List[Waiter] = List()
    private var posedgeWaiters: List[Waiter] = List()
    private var negedgeWaiters: List[Waiter] = List()

    // get n th bit
    def apply(n: Int): Int = {
      if (n < length)
        (value / pow(2, n).toInt) & 1
      else 0
    }

    def setNext(n: Int) {
      if (signed) {
        next = if (HDLPrimitive.getSignedSize(n) > length) {
          val s = n.toBinaryString
          val t = Integer.parseInt(s.slice(s.length - length + 1, s.length), 2)
          if (s(s.length - length) == "1"(0)) -(~t + 1)
          else t
        } else {
          n
        }
      } else {
        if (n >= 0) {
          next = if (HDLPrimitive.getUnsignedSize(n) > length) {
            val s = n.toBinaryString
            Integer.parseInt(s.slice(s.length - length, s.length), 2)
          } else n
        } else {
          var n0 = n
          while (n0 < 0) {
            n0 += pow(2, length).toInt
          }
          next = n0
        }
      }
    }

    def addWaiter(w: Waiter) {
      eventWaiters = w :: eventWaiters
    }

    def addWaiter(w: Waiter, v: Int) {
      if (v == 1) posedgeWaiters = w :: posedgeWaiters
      else negedgeWaiters = w :: negedgeWaiters
    }

    def needUpdate: Boolean =
      next != value

    def update: List[Waiter] = {
      if (needUpdate) {
        var lst = eventWaiters
        if (value < next)
          lst = posedgeWaiters ::: lst
        else if (value > next)
          lst = negedgeWaiters ::: lst
        value = next
        lst
      }
      else List()
    }

    override def toString =
      name + "(" + value + ", " + length + ")"
  }

  class RegisterBit(override val name: String, _value: Int,
    reg: Register, idx: Int)
      extends Register(name, _value, 1, false) {

    def getReg = reg

    override def setNext(n: Int) {
      if (n < 2 && n >= 0) {
        if (next != n) {
          next = n
        }
      }
    }

    // Not used!
    override def update: List[Waiter] = {
      if (needUpdate) {
        value = next
        reg.setNext(reg.value ^ (1 << idx))
        reg.update
      }
      else List()
    }

    override def toString =
      reg.toString + "(" + idx + ":" + value + ")"
  }

  trait Arithable

  case class HDLIndex[T](a: HDLDef[T], idx: Int) extends HDLDef[T] {
    override def registers: List[Register] = a.registers.map {
      r => new RegisterBit(r.name, (r.value >> idx) & 1, r, idx)
    }
  }

  case class HDLSlice[T](a: HDLExp[T], hi: Int, lo: Int) extends HDLDef[T] {
    override def registers: List[Register] = a.registers
  }

  case class HDLValueList[T](lst: List[HDLExp[T]]) extends HDLDef[T] {
    override def registers: List[Register] = lst.flatMap(_.registers)
    def apply(idx: HDLReg[Unsigned]) = HDLValueListElem(this, idx)
  }

  case class HDLValueListElem[T](lst: HDLValueList[T], idx: HDLReg[Unsigned])
      extends HDLDef[T] {
    override def registers: List[Register] = lst.registers
  }

  // Bitweise operations

  case class HDLBitwiseAnd[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLBitwiseOr[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLBitwiseXor[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  // Arithmetic operations

  case class HDLAdd[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLSub[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLMul[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLDiv[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLMod[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLConcat[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[T]

  case class HDLLeftShift[+T](a: HDLExp[T], b: HDLExp[Unsigned])
      extends HDLExp[T]

  case class HDLRightShift[+T](a: HDLExp[T], b: HDLExp[Unsigned])
      extends HDLExp[T]

  case class HDLLeftShiftInt[+T](a: HDLExp[T], b: Int) extends HDLExp[T]

  case class HDLRightShiftInt[+T](a: HDLExp[T], b: Int) extends HDLExp[T]

  case class HDLGreaterThan[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[Boolean]

  case class HDLLessThan[T](a: HDLExp[T], b: HDLExp[T]) extends HDLExp[Boolean]

  case class HDLGreaterThanOrEqual[T](a: HDLExp[T], b: HDLExp[T])
      extends HDLExp[Boolean]

  case class HDLLessThanOrEqual[T](a: HDLExp[T], b: HDLExp[T])
      extends HDLExp[Boolean]

  abstract class HDLType {
    def toRegisters: List[Register]
    def toRegisters(name: String): List[Register]
  }

  // C means custom type
  abstract class HDLCType extends HDLType

  abstract class HDLPrimitive(
    val value: Int, val length: Int, val signed: Boolean) extends HDLType {

    checkValid()

    protected def checkValid() {
      if (!signed) {
        if (value < 0)
          throw new IllegalArgumentException("the value cannot be less than 0")
        val expected = HDLPrimitive.getUnsignedSize(value)
        if (expected > length)
          throw new NotEnoughBitsException(getName, value, expected, length)
      } else {
        val expected = HDLPrimitive.getSignedSize(value)
        if (expected > length)
          throw new NotEnoughBitsException(getName, value, expected, length)
      }
    }

    def getName: String

    override def toRegisters = List(
      new Register(getName, value, length, false))
    override def toRegisters(name: String) = List(
      new Register(name, value, length, false))
  }

  object HDLPrimitive {
    def getUnsignedSize(value: Int): Int = {
      value.abs.toBinaryString.length
    }

    def getSignedSize(value: Int): Int = {
      val s = value.toBinaryString
      if (value == -1) 2
      else if (value < 0) ("1" + s.dropWhile(_ == '1')).length
      else if (value == 0) 1
      else s.length + 1
    }
  }

  case class Bool(override val value: Int)
      extends HDLPrimitive(value, 1, false) with Arithable {
    override def getName = "Bool(" + value + ")"
  }

  case class Signed(override val value: Int, override val length: Int)
      extends HDLPrimitive(value, length, true) with Arithable {
    override def getName = "Signed(" + value + ")"
    override def toRegisters = List(
      new Register(getName, value, length, true))
    override def toRegisters(name: String) = List(
      new Register(name, value, length, true))
  }

  case class Unsigned(override val value: Int, override val length: Int)
      extends HDLPrimitive(value, length, false) with Arithable {
    override def getName = "Unsigned(" + value + ")"
  }

  case class ComplexNumber(valueA: Int, lengthA: Int, valueB: Int, lengthB: Int) extends HDLCType with Arithable {
    def getName: String = "ComplexNumber(" + valueA + "+" + valueB + "i)"
    var A = Signed(valueA, lengthA)
    var B = Signed(valueB, lengthB)
    var regA = HDL(A)
    var regB = HDL(B)
    def getRegA = regA
    def getRegB = regB

    override def toString = getName

    override def toRegisters = List(
      A.toRegisters.head, B.toRegisters.head)
    override def toRegisters(name: String) = List(
      A.toRegisters(name+"A").head, B.toRegisters(name+"B").head)

  }

  implicit def list2hdlvaluelist[T](x: List[T]): HDLValueList[T] =
    HDLValueList(x.map(any2hdl(_)))
  implicit def any2hdl[T](x: => T): HDLReg[T] = new HDLReg[T](x)
  implicit def any2chdl[T <: HDLCType](x: => T): HDLCTypeReg[T] = new HDLCTypeReg[T](x)
  implicit def int2hdlsigned(x: => Int): HDLReg[Signed] = new HDLReg(Signed(x,
    HDLPrimitive.getSignedSize(x)))

  private var exps: List[HDLExp[Any]] = List()
  private val expStack: Stack[List[HDLExp[Any]]] = new Stack()
  def addExp(exp: HDLExp[Any]) = {
    var lst = expStack.pop()
    lst = exp :: lst
    expStack.push(lst)
    expStack
  }
  def replaceLastExp(exp: HDLExp[Any]) = {
    var lst = expStack.pop()
    lst = exp :: lst.tail
    expStack.push(lst)
    expStack
  }
  def removeLastExp() = {
    val lst = expStack.pop()
    expStack.push(lst.tail)
    expStack
  }
  def getExps = expStack.top.reverse
  def incExpLvl() {
    expStack.push(List())
  }
  def clearExps() {
    expStack.pop()
  }
  def getAndClearExps = {
    val l = getExps
    clearExps()
    l
  }

  abstract class HDLExp[+T] {
    def is[S >: T](other: HDLExp[S]) =
      HDLEquals[S](this, other)

    def isnot[S >: T](other: HDLExp[S]) =
      HDLNotEquals[S](this, other)

    def unary_~[S >: T] = HDLRev[S](this)

    def +[S >: T](another: HDLExp[S]) = HDLAdd(this, another)
    def -[S >: T](another: HDLExp[S]) = HDLSub(this, another)
    def *[S >: T](another: HDLExp[S]) = HDLMul(this, another)
    def /[S >: T](another: HDLExp[S]) = HDLDiv(this, another)
    def %[S >: T](another: HDLExp[S]) = HDLMod(this, another)
    def ~~[S >: T](another: HDLExp[S]) = HDLConcat(this, another)

    def &[S >: T](another: HDLExp[S]) = HDLBitwiseAnd(this, another)
    def |[S >: T](another: HDLExp[S]) = HDLBitwiseOr(this, another)
    def ^[S >: T](another: HDLExp[S]) = HDLBitwiseXor(this, another)

    def >>(another: HDLExp[Unsigned]) = HDLRightShift(this, another)
    def <<(another: HDLExp[Unsigned]) = HDLLeftShift(this, another)
    def >>(another: Int) = HDLRightShiftInt(this, another)
    def <<(another: Int) = HDLLeftShiftInt(this, another)

    def >[S >: T](another: HDLExp[S]) = HDLGreaterThan(this, another)
    def <[S >: T](another: HDLExp[S]) = HDLLessThan(this, another)
    def >=[S >: T](another: HDLExp[S]) = HDLGreaterThanOrEqual(this, another)
    def <=[S >: T](another: HDLExp[S]) = HDLLessThanOrEqual(this, another)
  }

  case class HDLAssign[T](lhs: HDLDef[T], rhs: HDLExp[T])
      extends HDLExp[T]

  case class HDLEquals[T](lhs: HDLExp[T], rhs: HDLExp[T])
      extends HDLExp[Boolean]

  case class HDLNotEquals[T](lhs: HDLExp[T], rhs: HDLExp[T])
      extends HDLExp[Boolean]

  abstract class HDLCondition[T] extends HDLExp[T]

  case class HDLNormalCondition[T](cond: HDLExp[Boolean], f: Seq[HDLExp[T]])
      extends HDLCondition[T]

  case class HDLBooleanCondition[T](cond: Boolean, f: Seq[HDLExp[T]])
      extends HDLCondition[T]

  case class HDLWhen[T](conditions: Seq[HDLCondition[T]]) extends HDLExp[T]

  abstract class HDLDef[+T] extends HDLExp[T] {
    def :=[S >: T](rhs: HDLExp[S]) = {
      val a = HDLAssign[S](this, rhs)
      addExp(a)
      a
    }

    def registers: List[Register]
  }

  class HDLReg[+T](_value: => T)
      extends HDLDef[T] {

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

    def isConst = name.isEmpty

    def value = hdlval(_value)

    override def :=[S >: T](rhs: HDLExp[S]) = {
      out = true
      val a = HDLAssign[S](this, rhs)
      addExp(a)
      a
    }

    def length: Int = _value match {
      case Seq(a, _*) => a.length
      case p: HDLPrimitive => p.length
      case b: Boolean => 1
      case r: HDLCTypeReg[ComplexNumber] => 1
    }

    def lengthString =
      if (length > 1) "[" + (length - 1) + ":0] "
      else ""

    def signed: Boolean = _value match {
      case Seq(a, _*) =>
        a.signed
      case p: HDLPrimitive => p.signed
      case b: Boolean => false
      case r: HDLCTypeReg[ComplexNumber] => false
    }

    def signedString =
      if (signed) "signed " else ""

    def sizeString = _value match {
      case s: Seq[Any] => " [0:" + (s.length-1) + "]"
      case _ => ""
    }

    def apply[S >: T](idx: Int): HDLIndex[S] = HDLIndex[S](this, idx)

    def apply[S >: T](lo: Int, hi: Int): HDLSlice[S] = HDLSlice[S](this, hi, lo)

    def apply[S >: T](idx: HDLExp[Unsigned]): HDLListElem[S] =
      HDLListElem(this, idx)

    override def toString = "HDLReg " + getName

    def initDecl: String = _value match {
      case s: Seq[Any] =>
        val a = s.indices.zip(s.map(any2hdl(_))).map(elt =>
          getName + "[" + elt._1 + "] = " + elt._2.value).mkString(";\n") + ";\n"
        a
      case _ =>
        getName + " = " + value + ";\n"
    }

    // for simulation purpose
    protected var corresRegs: Option[List[Register]] = None

    override def registers: List[Register] =
      corresRegs match {
        case Some(theReg) =>
          theReg
        case None =>
          val v = _value
          name match {
            case None =>
              val theReg = v match {
                case p: HDLType => p.toRegisters
                case b: Boolean => List(
                  new Register("", if (b) 1 else 0, 1, false))
                case _ => List()
              }
              theReg
            case Some(nm) =>
              val theReg = v match {
                case p: HDLType => p.toRegisters(nm)
                case b: Boolean => List(
                  new Register(nm, if (b) 1 else 0, 1, false))
                case _ => List()
              }
              corresRegs = Some(theReg)
              theReg
          }
      }

    override def equals(other: Any): Boolean = other match {
      case num: Int =>
        registers.size == 1 && registers.head.value == num
      case reg: HDLReg[T] =>
        getName == reg.getName && value == reg.value &&
        length == reg.length && signed == reg.signed
    }
  }

  // CType means custom type
  class HDLCTypeReg[+T](_value: => T)
      extends HDLDef[T] {

    var name: Vector[String] = Vector.empty
    var out: Boolean = false
    var reg: Boolean = true
    var exist: Boolean = true

    def setName(_name: String) = _value match {
      case _value: ComplexNumber =>
        name = Vector[String](_name, _name+"A", _name+"B")
    }

    def getName(index: Int) = _value match {
      case _value: ComplexNumber => {
        if (name.length < 3) {
          _value.toString
        } else if (index <= 2) {
          name(index)
        } else {
          _value.toString
        }
      }
    }

    def getAllName = _value match {
      case _value: ComplexNumber =>
        List[String](name(1), name(2))
    }

    override def toString = "HDLCTypeReg " + getName(0)

    def getReg(index: Int) = _value match {
      case _value: ComplexNumber => {
        if (index == 0) {
          _value.getRegA
        } else if (index == 1) {
          _value.getRegB
        }
      }
    }

    def signedString(index: Int) = _value match {
      case _value: ComplexNumber => {
        if (index == 0 && _value.getRegA.signed) {
          "signed"
        } else if (index == 1 && _value.getRegB.signed) {
          "signed"
        } else {
          ""
        }
      }
    }

    def lengthString(index: Int) = _value match {
      case _value: ComplexNumber => {
        if (index == 0) {
          _value.getRegA.lengthString
        } else if (index == 1) {
          _value.getRegB.lengthString
        } else {
          ""
        }
      }
    }

    override def :=[S >: T](rhs: HDLExp[S]) = {
      out = true
      val a = HDLAssign[S](this, rhs)
      addExp(a)
      a
    }

    def registers: List[Register] = List()
  }

  case class HDLListElem[T](lst: HDLReg[T], idx: HDLExp[Unsigned])
      extends HDLDef[T] {
    override def registers: List[Register] = lst.registers
  }

  abstract class HDLBlock(val exps: Seq[HDLExp[Any]])

  case class HDLSyncBlock(reg: HDLReg[Boolean], when: Int,
    override val exps: Seq[HDLExp[Any]])
      extends HDLBlock(exps)

  case class HDLMultSyncBlock(regWhen: Seq[(HDLReg[Boolean], Int)],
    override val exps: Seq[HDLExp[Any]])
      extends HDLBlock(exps)

  case class HDLAsyncBlock(senslist: Seq[HDLReg[Any]],
    override val exps: Seq[HDLExp[Any]])
      extends HDLBlock(exps)

  case class HDLDelayBlock(duration: Int,
    override val exps: Seq[HDLExp[Any]])
      extends HDLBlock(exps)

  class HDLModule(_name: String) {

    val name = _name
    private var _params: List[HDLReg[Any]] = List()
    private var _cparams: List[HDLCTypeReg[Any]] = List()
    private var _blocks: List[HDLBlock] = List()
    private var _externalModule: List[HDLClass] = List()
    var internalRegs: List[HDLReg[Any]] = List()
    var analyzed = false

    private var regCounter = 0

/*
    def HDLlize[T](x: Seq[T]): HDLList[T] = {
      val l = new HDLList(x.map(any2hdl(_)), "temp" + regCounter)
      internalRegs = l :: internalRegs
      regCounter += 1
      l
    }
 */

    def HDLlize[T](x: T): HDLReg[T] = {
      val r = new HDLReg(x)
      r.setName("temp" + regCounter)
      internalRegs = r :: internalRegs
      regCounter += 1
      r
    }

    def setParams(params: List[Any]) {
      params.foreach {
        case reg: HDLReg[Any] =>
          _params = reg :: _params
        case creg: HDLCTypeReg[Any] =>
          _cparams = creg :: _cparams
      }
    }

    def params = _params.reverse

    def cparams = _cparams.reverse

    def setBlocks(blocks: List[HDLBlock]) {
//      _blocks = blocks.reverse
    }

    def blocks = _blocks.reverse

    def addBlock(block: HDLBlock) {
      _blocks = block :: _blocks
    }

    def addExternalModule(externalModule: HDLClass): HDLModule = {
      _externalModule = externalModule :: _externalModule
      this
    }

    def externalModule = _externalModule
  }

  def moduleImpl(c: blackbox.Context)(blocks: c.Expr[HDLBlock]*):
      c.Expr[HDLModule] = {
    import c.universe._
    def constructModule(moduleName: Name, names: List[(TermName, Int)]) = {
      // set name for parameters
      val l = names.map(pair => {
        val name = pair._1
        val tpe = pair._2
        if (tpe == 0) {
          Apply(Select(
            Ident(TermName(name.toString)), TermName("setName")),
            List(Literal(Constant(name.toString))))
        } else {
          Apply(Select(
            Apply(Select(Literal(Constant(0)), TermName("until")),
              List(Select(Ident(TermName(name.toString)),
                TermName("size")))), TermName("foreach")),
            List(Function(List(ValDef(Modifiers(Flag.PARAM),
              TermName("i"), TypeTree(), EmptyTree)),
              Apply(Select(
                Apply(Select(Ident(TermName(name.toString)),
                  TermName("apply")), List(Ident(TermName("i")))),
                TermName("setName")),
                List(Apply(Select(Literal(Constant(name.toString)),
                  TermName("$plus")), List(Ident(TermName("i")))))))))
        }})
      val r = Apply(Select(New(Ident(TypeName("HDLModule"))),
        termNames.CONSTRUCTOR),
        List(Literal(Constant(moduleName.decodedName.toString))))
      val mod = TermName("mod")
      val d = ValDef(Modifiers(), mod, TypeTree(), r)
      val p = Apply(Select(Ident(mod), TermName("setParams")),
        List(Apply(Select(Ident(TermName("List")), TermName("apply")),
          names.map(pair =>
            Ident(pair._1)))))
      val b = Apply(Select(Ident(mod), TermName("setBlocks")),
        List(Apply(Select(Ident(TermName("List")), TermName("apply")),
          blocks.map(_.tree).toList)))
      val db = Apply(Apply(Select(Ident(TermName("currentMod")),
        TermName("withValue")), List(Ident(mod))), List(Block(List(p), b)))
      c.Expr[HDLModule](Block(l ++ List(r, d, db), Ident(mod)))
    }

    // replace "enclosingMethod" with "enclosingDef"
    // in the future version of Scala!
    c.enclosingMethod match {
      case DefDef(_, moduleName, _, List(), _, _) =>
        var names: List[(TermName, Int)] = List()
        c.enclosingClass match {
          case ClassDef(_, className, _, Template(_, _, params)) =>
            params.foreach {
              case DefDef(_, name, _, params, _, _) =>
                if (name == termNames.CONSTRUCTOR)
                  params.head.map(
                    (param) => param match {
                      case ValDef(_, name,
                      AppliedTypeTree(Ident(typeName), inner), _) =>
                        // 0 for HDL[T]
                        if (typeName == TypeName("HDL") || typeName == TypeName("CHDL"))
                          names = (name, 0) :: names
                        // 1 for List[HDL[T]]
                        else if (typeName == TypeName("List"))
                          inner match {
                            case List(AppliedTypeTree(Ident(typeName2), _)) =>
                              if (typeName2 == TypeName("HDL") || typeName == TypeName("CHDL"))
                                names = (name, 1) :: names
                            case _ => ()
                          }
                      case _ => ()
                    })
              case _ => ()
            }
            constructModule(moduleName, names.reverse)
        }
      case DefDef(_, moduleName, _, params, _, _) =>
        val names = params.head.map((param) => param match {
          case ValDef(_, name, _, _) => name
        })
        constructModule(moduleName, names.map(name => (name, 0)))
      case _ =>
        c.Expr[HDLModule](
          Apply(Select(New(Ident(TypeName("HDLModule"))),
            termNames.CONSTRUCTOR),
            List(Literal(Constant("")), Literal(Constant(null)))))
    }
  }

  abstract class HDLBaseClass

  //abstract class HDLClass extends HDLBaseClass with BasicOps with Compiler with NetworkOps
  abstract class HDLClass extends HDLBaseClass with BasicOps with Compiler

  class CompileOutput(genCode: String) {
    override def toString = genCode

    def toConsole: CompileOutput = {
      println(genCode)
      this
    }

    def toFile(filename: String): CompileOutput = {
      val writer = new PrintWriter(new File(filename))
      writer.print(genCode)
      writer.close
      this
    }
  }

  trait Base {
    // Arithmetic related.

    def module(blocks: HDLBlock*): HDLModule = macro moduleImpl

    protected def getSenslist(exp: HDLExp[Any]): Seq[HDLReg[Any]] = exp match {
      case HDLWhen(conditions) =>
        conditions.map(getSenslist(_)).reduceLeft((l, r) => l ++ r)
      case HDLNormalCondition(c, f) =>
        getSenslist(c) ++ getSenslist(f)
      case HDLBooleanCondition(_, f) => getSenslist(f)
      case HDLEquals(l, r) => getSenslist(l) ++ getSenslist(r)
      case HDLNotEquals(l, r) => getSenslist(l) ++ getSenslist(r)
      case HDLAssign(_, rhs) => getSenslist(rhs)
      case r: HDLReg[Any] => if (!r.isConst) Seq(r) else Seq()
      case HDLRev(x) => getSenslist(x)
      case HDLAdd(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLSub(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLMul(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLDiv(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLBitwiseAnd(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLBitwiseOr(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLBitwiseXor(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLGreaterThan(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLLessThan(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLGreaterThanOrEqual(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLLessThanOrEqual(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLConcat(x, y) => getSenslist(x) ++ getSenslist(y)
      case HDLIndex(x, _) => getSenslist(x)
      case HDLSlice(x, _, _) => getSenslist(x)
    }

    private def getSenslist(exps: Seq[HDLExp[Any]]): Seq[HDLReg[Any]] = {
      (for (exp <- exps) yield getSenslist(exp)).flatten
    }

    def sync(clk: HDLReg[Boolean], when: Int) = {
      incExpLvl()
      HDLSyncPart(clk, when)
    }

    case class HDLSyncPart(clk: HDLReg[Boolean], when: Int) {
      def apply(exps: HDLExp[Any]*) = {
        val block = HDLSyncBlock(clk, when, getAndClearExps)
        currentMod.value.addBlock(block)
        block
      }
    }

    def sync(clkWhen: (HDLReg[Boolean], Int)*) = {
      incExpLvl()
      HDLMultSyncPart(clkWhen)
    }

    case class HDLMultSyncPart(clkWhen: Seq[(HDLReg[Boolean], Int)]) {
      def apply(exps: HDLExp[Any]*) = {
        val block = HDLMultSyncBlock(clkWhen, getAndClearExps)
        currentMod.value.addBlock(block)
        block
      }
    }

    def async = {
      incExpLvl()
      new HDLAsyncPart
    }

    class HDLAsyncPart {
      def apply(exps: HDLExp[Any]*) = {
        val exps = getAndClearExps
        val senslist = getSenslist(exps).distinct
        val block = HDLAsyncBlock(senslist, exps)
        currentMod.value.addBlock(block)
        block
      }
    }

    def delay(duration: Int) = {
      incExpLvl()
      HDLDelayPart(duration)
    }

    case class HDLDelayPart(duration: Int) {
      def apply(exps: HDLExp[Any]*) = {
        val block = HDLDelayBlock(duration, getAndClearExps)
        currentMod.value.addBlock(block)
        block
      }
    }

    def when(cond: HDLExp[Boolean]) = {
      incExpLvl()
      WhenPart1(cond)
    }

    case class WhenPart1(cond: HDLExp[Boolean]) {
      def apply(exps: Unit*) = {
        val p = new WhenPart2(List(
          HDLNormalCondition(cond, getAndClearExps)))
        p
      }
    }

    class WhenPart2(conds: List[HDLCondition[Any]]) {

      addExp(HDLWhen(conds))

      object HDLOtherwise {
        def apply(exps: Unit*) = {
          val fal = getAndClearExps
          val w = HDLWhen(HDLBooleanCondition(true, fal) :: conds)
          replaceLastExp(w)
          w
        }
      }

      object HDLElsewhen {
        def apply(cond: HDLExp[Boolean])(exps: Unit*) = {
          val oth = getAndClearExps
          val conds2 = HDLNormalCondition(cond, oth) :: conds
          val w = HDLWhen(conds2)
          removeLastExp()
          new WhenPart2(conds2)
        }
      }

      def otherwise = {
        incExpLvl()
        HDLOtherwise
      }

      def elsewhen = {
        incExpLvl()
        HDLElsewhen
      }
    }
  }

  trait BasicOps extends Base {
  }

  trait Compiler extends Base {
    val toCompile: List[HDLModule] = List()

    def compile: CompileOutput =
      new CompileOutput((for (module <- toCompile) yield compile(module)).mkString(""))

    protected def compile[T](exp: HDLExp[T]): String = exp match {
      case HDLWhen(conditions) =>
        conditions.reverse.map(compile(_)).mkString("\nelse ")
      case HDLNormalCondition(c, f) => c match {
        case r: HDLReg[Any] =>
          "if (" + compile(c) + " == 1) begin\n" +
          f.map(compile(_)).mkString("\n") + "\nend"
        case _ =>
          println(c)
          "if (" + compile(c) + ") begin\n" +
          f.map(compile(_)).mkString("\n") + "\nend"
      }
      case HDLBooleanCondition(b, f) if b =>
        "begin\n" + f.map(compile(_)).mkString("\n") + "\nend\n"
      case HDLEquals(l, r) =>
        compile(l) + " == " + compile(r)
      case HDLNotEquals(l, r) =>
        compile(l) + " != " + compile(r)
      case HDLAssign(lhs, rhs) =>
        (lhs, rhs) match {
          case (lhs: HDLCTypeReg[ComplexNumber], rhs: HDLExp[HDLCTypeReg[ComplexNumber]]) =>
            rhs match {
              case HDLAdd(x: HDLCTypeReg[ComplexNumber], y: HDLCTypeReg[ComplexNumber]) =>
                lhs.getName(1) + " <= (" + x.getName(1) + " + " + y.getName(1) + ");\n" +
                lhs.getName(2) + " <= (" + x.getName(2) + " + " + y.getName(2) + ");"
              case HDLSub(x: HDLCTypeReg[ComplexNumber], y: HDLCTypeReg[ComplexNumber]) =>
                lhs.getName(1) + " <= (" + x.getName(1) + " - " + y.getName(1) + ");\n" +
                lhs.getName(2) + " <= (" + x.getName(2) + " - " + y.getName(2) + ");"
              case HDLMul(x: HDLCTypeReg[ComplexNumber], y: HDLCTypeReg[ComplexNumber]) =>
                lhs.getName(1) + " <= (" + x.getName(1) + " * " + y.getName(1) + " - " + x.getName(2) + " * " + y.getName(2) +");\n" +
                lhs.getName(2) + " <= (" + x.getName(2) + " * " + y.getName(1) + " + " + x.getName(1) + " * " + y.getName(2) +");"
            }
          case (_, HDLValueListElem(lst, idx)) =>
            val l = lst.lst
            val s = l.indices.map(i =>
              List(i, ": ", compile(lhs),
                " <= ", compile(l(i)), ";").mkString).mkString("\n")
            "case (" + idx.getName + ")\n" + s + "\nendcase\n"
          case _ =>
            compile(lhs) + " <= " + compile(rhs) + ";"
        }
      case HDLRev(x) =>
        "~" + compile(x)
      case HDLAdd(x, y) =>
        "(" + compile(x) + " + " + compile(y) + ")"
      case HDLSub(x, y) =>
        "(" + compile(x) + " - " + compile(y) + ")"
      case HDLMul(x, y) =>
        "(" + compile(x) + " * " + compile(y) + ")"
      case HDLDiv(x, y) =>
        "(" + compile(x) + " / " + compile(y) + ")"
      case HDLMod(x, y) =>
        "(" + compile(x) + " % " + compile(y) + ")"
      case HDLBitwiseAnd(x, y) =>
        "(" + compile(x) + " & " + compile(y) + ")"
      case HDLBitwiseOr(x, y) =>
        "(" + compile(x) + " | " + compile(y) + ")"
      case HDLBitwiseXor(x, y) =>
        "(" + compile(x) + " ^ " + compile(y) + ")"
      case HDLGreaterThan(x, y) =>
        "(" + compile(x) + " > " + compile(y) + ")"
      case HDLLessThan(x, y) =>
        "(" + compile(x) + " < " + compile(y) + ")"
      case HDLGreaterThanOrEqual(x, y) =>
        "(" + compile(x) + " >= " + compile(y) + ")"
      case HDLLessThanOrEqual(x, y) =>
        "(" + compile(x) + " <= " + compile(y) + ")"
      case HDLLeftShift(x, y) =>
        "(" + compile(x) + " << " + compile(y) + ")"
      case HDLRightShift(x, y) =>
        "(" + compile(x) + " >> " + compile(y) + ")"
      case HDLLeftShiftInt(x, y) =>
        "(" + compile(x) + " << " + y + ")"
      case HDLRightShiftInt(x, y) =>
        "(" + compile(x) + " >> " + y + ")"
      case HDLConcat(x, y) =>
        "{" + compile(x) + "," + compile(y) + "}"
      case HDLIndex(x, idx) =>
        compile(x) + "[" + idx + "]"
      case HDLSlice(x, hi, lo) =>
        compile(x) + List("[", hi - 1, ":", lo, "]").mkString("")
      case HDLListElem(lst, idx) =>
        compile(lst) + "[" + compile(idx) + "]"
      case r: HDLReg[T] =>
        r.getName
    }

    protected def compile(b: HDLBlock): String = {
      val stmts = (for (exp <- b.exps) yield compile(exp)).mkString("\n")
      b match {
        case HDLSyncBlock(reg, when, _) =>
          "always @(" + (if (when == 1) "posedge " else "negedge ") +
          reg.getName + ") begin\n" + stmts + "\nend\n"
        case HDLMultSyncBlock(regs, _) =>
          "always @(" + regs.map(regWhen =>
            (if (regWhen._2 == 1) "posedge " else "negedge ")
              + regWhen._1.getName).mkString(
            " or ") + ") begin\n" + stmts + "\nend\n"
        case HDLAsyncBlock(senslist, _) =>
          "always @(" + senslist.map(compile(_)).mkString(", ") +
          ") begin\n" + stmts + "\nend\n"
      }
    }

    protected def moduleDeclaration(m: HDLModule, content: String): String = {
      val paramNames = m.params.map(_.getName)
      var cparamNames = m.cparams.map(_.getAllName)
      var allParamNames = paramNames
      while (cparamNames.nonEmpty) {
        allParamNames = allParamNames ++ cparamNames.head
        cparamNames = cparamNames.tail
      }

      List("module ", m.name,
        "(\n", allParamNames.mkString(",\n"), "\n);\n\n",
        content, "\nendmodule\n").mkString("")
    }

    protected def registerDeclaration(m: HDLModule): String = {
      val regs = m.params.filter(_.out).filter(_.reg)
      m.params.map((p) =>
        (if (p.out) "output " else "input ")
          + p.signedString + p.lengthString + p.getName +
          ";\n").sorted.mkString("") +
      m.cparams.map((p) =>
          (if (p.out) "output " else "input ") +
          p.signedString(0) + p.lengthString(0) + p.getName(1) + ";\n" +
          (if (p.out) "output " else "input ") +
          p.signedString(1) + p.lengthString(1) + p.getName(2) + ";\n"
      ).sorted.mkString("") +
      regs.map(p => "reg " +
        p.signedString + p.lengthString +
        p.getName + p.sizeString + ";\n").sorted.mkString("") +
      m.internalRegs.map((r) =>
        "reg " + r.signedString + r.lengthString + r.getName + r.sizeString +
          ";\n").sorted.mkString("") +
      "\ninitial begin\n" + (regs ++ m.internalRegs).map(
        _.initDecl).sorted.mkString("") + "end\n\n"
    }


    protected def refExternalModule(m: List[HDLClass]): String = {
      val HDLRegInstance = HDL(false)
      val RegType: String = HDLRegInstance.getClass.getTypeName
      m.map((p) =>
        p.getClass.getSimpleName +
        "(" +
        p.getClass.getDeclaredFields.map((f) =>
          if (f.getType.getName == RegType) "somePara" else "").mkString(",") +
        ")" +
        "\n"
      ).mkString("")
    }


    def compile(m: HDLModule): String = {
      moduleDeclaration(m,
        registerDeclaration(m) + refExternalModule(m.externalModule) + "\n" +
        (for (block <- m.blocks) yield compile(block)).mkString("\n"))
    }
  }

  trait NetworkOps {

    var network_on_off: Boolean = true

    // send about val and var
    val network_send_max_byte: Int = 512
    var network_send_ip = InetAddress.getByName("0.0.0.0")
    var network_send_port = 0
    val network_send_socket = new DatagramSocket()

    // receive about val and var
    val network_receive_buffer = new Array[Byte](512)
    var network_receive_port = 15000
    var network_receive_socket = new DatagramSocket(network_receive_port)

    // use for first connect overtime judge
    // TODO: conside this case, not robust enough now
    val network_connect_overtime = 10000
    var network_connect_times = 0



    // send function
    def network_send(message: String): Unit = {
      val sub_message = if (message.length() > network_send_max_byte) message.substring(0, network_send_max_byte) else message
      println(s"[ScalaHDL] sending: $sub_message")
      val network_send_buffer = sub_message.getBytes("utf-8")
      val network_send_packet = new DatagramPacket(network_send_buffer, network_send_buffer.length, network_send_ip, network_send_port)
      network_send_socket.send(network_send_packet)
      if (sub_message != message) network_send(message.substring(network_send_max_byte))
    }


    // receive loop function
    @tailrec
    final def network_receive_loop(_socket: DatagramSocket, _receive_buffer: Array[Byte]) : Unit= {
      val receive_packet = new DatagramPacket(_receive_buffer, _receive_buffer.length)
      _socket.receive(receive_packet) // loop block!
      val receive_data = new String(receive_packet.getData, 0, receive_packet.getLength, "utf-8")
      val sender_ip = receive_packet.getAddress
      val sender_port = receive_packet.getPort
      println(s"[ScalaHDL] from: $sender_ip:$sender_port, message: $receive_data")

      // new thread for handle function
      new Thread(new Runnable {
        def run() {
          if (sender_ip != network_send_ip) {
            println("[ScalaHDL] ***** Unlikelihood Message *****")
          } else {
            network_receive_handle(receive_data)
          }
        }
      }).run()

      if (!receive_data.equals("CLOSE"))  // temp demo code
        network_receive_loop(_socket, _receive_buffer)
      else
        network_send("CLOSE")
    }


    // receive handle function
    def network_receive_handle(message: String) : Unit = {
      println(s"handle: $message")
      // do some thing
      if (message.equals("aaa"))
        network_send("[ScalaHDL] send by reiceive_handle")

    }


    def network_debug_run(_ip : String, _send_port : Int, _receive_port : Int) : Unit = {
      if (network_on_off) {
        network_send_ip = InetAddress.getByName(_ip)
        network_send_port = _send_port
        network_receive_port = _receive_port
        network_receive_socket = new DatagramSocket(network_receive_port)
        network_send("START by ScalaHDL")
        network_receive_loop(network_receive_socket, network_receive_buffer)
      }
    }

    def network_debug_run() : Unit = {
      val conf : Config = ConfigFactory.load()
      network_debug_run(conf.getString("ScalaHDL.simulator-ip"), conf.getInt("ScalaHDL.simulator-port"),
        conf.getInt("ScalaHDL.ScalaHDL-port"))
    }
  }

}
