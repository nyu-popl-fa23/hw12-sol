package popl.js

import popl.js.ast.fv
import popl.js.util.JsException
import popl.js.util.State

import scala.util.parsing.input.Positional

object ast:
  /* JakartaScript Type Expressions */
  enum Typ:
    // pretty print as AST
    override def toString: String = print.prettyAST(this)
    // pretty print as JS expression
    def pretty: String = print.prettyTyp(this)
  
    case TNumber
    case TBool
    case TString
    case TUndefined
    case TFunction(ts: List[Typ], tret: Typ)
    case TObj(fts: Map[Fld, (Mut, Typ)])
    case TAny, TNothing

  /* Memory */
  class Mem private (map: Map[Addr, Con], nextAddr: Int):
    def apply(a: Addr): Con = map(a)
    def get(a: Addr): Option[Con] = map.get(a)
    def +(ak: (Addr, Con)): Mem = new Mem(map + ak, nextAddr)
    def contains(key: Addr): Boolean = map.contains(key)
    
    def alloc(k: Con): (Mem, Addr) =
      val fresha = Addr(nextAddr)
      (new Mem(map + (fresha -> k), nextAddr + 1), fresha)
    
    override def toString: String = map.toString
  
  object Mem:
    def empty = Mem(Map.empty, 1)
    def alloc(k: Con): State[Mem,Addr] = State(m => m.alloc(k))

  /* JakartaScript Expressions */
  sealed abstract class Expr extends Positional:
    // pretty print as AST
    override def toString: String = print.prettyAST(this)

    // pretty print as JS expression
    def prettyJS: String = print.prettyJS(this)

    // pretty print as value
    def prettyVal: String = print.prettyVal(this)
  end Expr

  /* Function Parameters */
  type Params = List[(String, Typ)]

  /* Memory contents */
  sealed abstract trait Con

  /* Objects (to be stored in memory) */
  case class Obj(fvs: Map[Fld, Val]) extends Con

  /* Literals and Values */
  sealed abstract class Val extends Expr with Con

  case class Num(n: Double) extends Val

  case class Bool(b: Boolean) extends Val

  case class Str(s: String) extends Val

  case object Undefined extends Val

  case class Addr private[ast] (a: Int) extends Val


  /* Variables */
  case class Var(x: String) extends Expr

  /* Variable declarations */
  case class Decl(mut: Mut, x: String, ed: Expr, eb: Expr) extends Expr

  /* Unary and Binary Operators */
  case class UnOp(op: Uop, e1: Expr) extends Expr

  case class BinOp(op: Bop, e1: Expr, e2: Expr) extends Expr

  /* Control constructs */
  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr

  /* I/O */
  case class Print(e1: Expr) extends Expr

  /* Functions */
  case class Function(p: Option[String], xs: Params, t: Option[Typ], e: Expr) extends Val

  /* Function Calls */
  case class Call(e0: Expr, es: List[Expr]) extends Expr

  /* Fields */
  type Fld = String

  /* Object literals */
  case class ObjLit(fes: Map[Fld, (Mut, Expr)]) extends Expr


  /* The above code is essentially equivalent to the following enum definitions given in the
   *  homework description, but behaves better with Scala's type inference.

  enum Expr extends Positional:
    // pretty print as AST
    override def toString: String = print.prettyAST(this)

    // Pretty print as JavaScript expression
    def prettyJS: String = print.prettyJS(this)

    // Pretty print expression as value
    def prettyVal: String = print.prettyVal(this)

    // Literals and values
    case Num(n: Double)
    case Bool(b: Boolean)
    case Str(s: String)
    case Undefined
    case Addr private[ast] (a: Int)

    // Variables
    case Var(x: String)

    // Variable declarations
    case Decl(mut: Mut, x: String, ed: Expr, eb: Expr)

    // Unary and binary operator expressions
    case UnOp(op: Uop, e1: Expr)
    case BinOp(op: Bop, e1: Expr, e2: Expr)

    /* Control constructs */
    case If(e1: Expr, e2: Expr, e3: Expr)

    /* I/O */
    case Print(e1: Expr)

    /* Functions */
    case Function(p: Option[String], xs: Params, t: Option[Typ], e: Expr)

    // Function calls
    case Call(e0: Expr, es: List[Expr])

    // Object literals
    case ObjLit(fes: Map[Fld, (Mut, Expr)])
 
  // Values
  type Val = Expr.Num | Expr.Bool | Expr.Str | Expr.Undefined.type | Expr.Function | Expr.Addr
  */

  // Mutabilities
  enum Mut:
    case MConst, MLet

  // Unary operators
  enum Uop:
    case UMinus, Not, Deref // - ! *
    /* Field dereference */
    case FldDeref(f: Fld) /* .f */
    

  // Binary operators
  enum Bop:
    case Plus, Minus, Times, Div // + - * /
    case Eq, Ne, Lt, Le, Gt, Ge // === !== < <= > >=
    case And, Or // && ||
    case Seq // ,
    case Assign // =

  /* Define values. */
  def isValue(e: Expr): Boolean = e match
    case _: Val => true
    case _ => false

  /* Define statements (used for pretty printing). */
  def isStmt(e: Expr): Boolean =
    import Bop._
    e match
      case Undefined | Decl(_, _, _, _) |
           Print(_) => true
      case BinOp(Seq, _, e2) => isStmt(e2)
      case _ => false

  /* Check whether expression contains a function */
  def hasFunction(e: Expr): Boolean = e match
    case Function(_, _, _, _) => true
    case BinOp(_, e1, e2) => hasFunction(e1) || hasFunction(e2)
    case UnOp(_, e1) => hasFunction(e1)
    case Print(e1) => hasFunction(e1)
    case Decl(_, _, e1, e2) => hasFunction(e1) || hasFunction(e2)
    case ObjLit(fs) => fs exists { case (_, (_, e)) => hasFunction(e) }
    case Call(e, args) => (e :: args) exists hasFunction
    case _ => false


  /* Get the free variables of e. */
  def fv(e: Expr): Set[String] =
    e match
      case Var(x) => Set(x)
      case Decl(_, x, ed, eb) => fv(ed) | (fv(eb) - x)
      case Num(_) | Bool(_) | Undefined | Str(_) | Addr(_) => Set.empty
      case UnOp(_, e1) => fv(e1)
      case BinOp(_, e1, e2) => fv(e1) | fv(e2)
      case If(e1, e2, e3) => fv(e1) | fv(e2) | fv(e3)
      case Print(e1) => fv(e1)
      case Call(e0, es) => fv(e0) | (es.toSet flatMap fv)
      case Function(p, xs, _, e) => fv(e) -- p -- xs.map(_._1)
      case ObjLit(fs) =>       
        fs.values.foldLeft(Set.empty: Set[String]){ case(acc, (_, e)) => acc | fv(e) }

  /* Check whether the given expression is closed. */
  def closed(e: Expr): Boolean = fv(e).isEmpty

  /* Pretty-print values. */
  def pretty(v: Val): String = v.toString

  /*
   * Static Type Error exception.  Throw this exception to signal a static
   * type error.
   * 
   *   throw StaticTypeError(tbad, e)
   * 
   */
  case class StaticTypeError(tbad: Typ, e: Expr) extends 
    JsException("Type Error: unexpected type: " + tbad.pretty, e.pos)

  /*
   * Location Expression Error exception.  Throw this exception 
   * to signal a location expression error.
   * 
   *   throw LocExprError(e)
   * 
   */
  case class LocTypeError(e: Expr) extends 
    JsException("Type Error: expected location expression", e.pos)

  /*
  * Stuck Type Error exception.  Throw this exception to signal getting
  * stuck in evaluation.  This exception should not get raised if
  * evaluating a well-typed expression.
  * 
  *   throw StuckError(e)
  * 
  */
  case class StuckError(e: Expr) extends JsException("stuck while evaluating expression", e.pos)

end ast
