package avocado

import scala.annotation.tailrec
import scala.quoted.*

object macros {

  def adoImpl[F[_]: Type, A: Type](compExpr: Expr[F[A]], apExpr: Expr[Applicative[F]])(using Quotes): Expr[F[A]] =
    ADOImpl(using quotes).adoImpl(compExpr, apExpr)

}

class ADOImpl(using Quotes) {
  import quotes.reflect.*

  private case class Binding(
    valdef: ValDef,
    tree: Term,
    methodName: String,
    typeArgs: List[TypeRepr],
    additionalArgs: List[Term]
  )

  def adoImpl[F[_]: Type, A: Type](compExpr: Expr[F[A]], apExpr: Expr[Applicative[F]])(using Quotes): Expr[F[A]] = {
    val exprTree = compExpr.asTerm match
      case Inlined(_, _, tree) => tree match
        case Block(Nil, expr) => expr
        case expr => expr
      case _ => throwGenericError()

    val (bindings, res) = toBindings(exprTree)

    val bindingVals: Set[ValDef] = bindings.map(_.valdef).toSet

    val bindingsWithDependencies: List[(Binding, Set[Symbol])] = bindings.map {
      case binding => (binding, getBindingDependencies(binding.tree, bindingVals))
    }

    connectBindings(bindingsWithDependencies, res, apExpr.asTerm).asExprOf[F[A]]
  }

  private def connectBindings(bindings: List[(Binding, Set[Symbol])], res: Term, ap: Term): Tree = {

    def go(bindings: List[(Binding, Set[Symbol])], zipped: List[ValDef], acc: Term, lastBinding: Binding): Term = bindings match {
      case Nil =>
        val term: Select = acc.select(acc.tpe.doWhatYouCanHACK.typeSymbol.methodMember(lastBinding.methodName).head)
        val tpes = lastBinding.typeArgs.map(_.widen)
        term
          .appliedToTypes(tpes)
          .appliedTo(funForZipped(zipped, res, Symbol.spliceOwner))
          .appliedToArgsIfNeeded(lastBinding.additionalArgs)
      case _ =>
        val (toZip, rest, newLastBinding) = splitToZip(bindings)
        val term: Select = acc.select(acc.tpe.typeSymbol.methodMember(lastBinding.methodName).head)
        val body = go(rest, toZip.map(_._1.valdef), zipExprs(toZip, ap), newLastBinding)
        val tpes = lastBinding.typeArgs.map(_.widen)
        term
          .appliedToTypes(tpes)
          .appliedTo(funForZipped(zipped, body, Symbol.spliceOwner))
          .appliedToArgsIfNeeded(lastBinding.additionalArgs)
    }

    val (toZip, rest, lastMethod) = splitToZip(bindings)
    go(rest, toZip.map(_._1.valdef), zipExprs(toZip, ap), lastMethod)
  }

  private def extractTypeFromApplicative(typeRepr: TypeRepr): TypeRepr = typeRepr.widen match {
    case AppliedType(_, args) => args.last
  }

  private def doZip(receiver: Term, valdef: ValDef, arg: Term, ap: Term): Term = {
    val receiverTypeSymbol = receiver.tpe.typeSymbol
    val argTpe = extractTypeFromApplicative(arg.tpe)
    ap
      .select(ap.tpe.typeSymbol.methodMember("zip").head)
      .appliedToTypes(List(valdef.tpt.tpe.widen, argTpe.widen)).appliedTo(receiver, arg)
  }

  private def zipExprs(toZip: List[(Binding, Set[Symbol])], ap: Term): Term = {
    toZip.init.foldRight(toZip.last._1.tree) {
      case ((binding, _), acc) =>
        doZip(binding.tree, binding.valdef, acc, ap)
    }
  }

  private def splitToZip(bindings: List[(Binding, Set[Symbol])]): (List[(Binding, Set[Symbol])], List[(Binding, Set[Symbol])], Binding) = {
    @tailrec
    def go(
      toZip: List[(Binding, Set[Symbol])],
      dropped: List[(Binding, Set[Symbol])],
      bindings: List[(Binding, Set[Symbol])],
      lastBinding: Binding
    ): (List[(Binding, Set[Symbol])], List[(Binding, Set[Symbol])], Binding) = bindings match {
      case Nil =>
        (toZip, dropped, lastBinding)
      case head :: bindings =>
        val (take, drop) = bindings.span(!_._2.contains(head._1.valdef.symbol))
        go(toZip :+ head, drop ++ dropped, take, head._1)
    }

    bindings match {
      case head :: _ if head._1.methodName == "flatMap" =>
        val (take, drop) = bindings.span(_._1.methodName == "flatMap")
        val (take1, drop1) = if drop.nonEmpty then (take :+ drop.head, drop.tail) else (take, drop)
        go(List.empty, drop1, take1, head._1)
      case head :: tail =>
        (List(head), tail, head._1)
      case _ =>
        throwGenericError()
    }
    
  }

  private def funForZipped(zipped: List[ValDef], body: Term, owner: Symbol): Term = {
    val tuple2: Term = '{Tuple2}.asTerm.asInstanceOf[Inlined].body
    val AppliedType(tuple2Tpe: TypeRepr, _) = TypeRepr.of[Tuple2[Any, Any]]: @unchecked

    def typeTreeOfTuples(zipped: List[ValDef]): TypeTree =
      TypeTree.of(using typeReprOfTuples(zipped).asType)

    def typeReprOfTuples(zipped: List[ValDef]): TypeRepr = zipped match {
      case Nil =>
        throwGenericError()
      case head :: Nil =>
        head.tpt.tpe
      case head :: zipped =>
        AppliedType(
          tuple2Tpe,
          List(
            head.tpt.tpe,
            typeReprOfTuples(zipped)
          )
        )
    }

    def makeBind(valdef: ValDef, owner: Symbol): (Tree, Map[Symbol, Symbol]) = {
      if valdef.name == "_" then
        Wildcard() -> Map.empty
      else
        val sym = Symbol.newBind(owner, valdef.name, Flags.EmptyFlags, valdef.tpt.tpe)
        Bind(sym, Wildcard()) -> Map(valdef.symbol -> sym)
    }

    def unapplies(zipped: List[ValDef], owner: Symbol): (Tree, Map[Symbol, Symbol]) = zipped match {
      case Nil =>
        throwGenericError()
      case head :: Nil =>
        makeBind(head, owner)
      case head :: zipped =>
        val (bind, renames) = makeBind(head, owner)
        val (tree, binds) = unapplies(zipped, owner)
        Unapply(
          TypeApply(
            Select(tuple2, tuple2.tpe.typeSymbol.methodMember("unapply").head),
            List(
              head.tpt,
              typeTreeOfTuples(zipped)
            )
          ),
          List.empty,
          List(
            bind,
            tree
          )
        ) -> (binds ++ renames)
    }

    val defdefSymbol = Symbol.newMethod(
      owner,
      "$anonfun",
      MethodType(List("syth$x$"))(_ => List(typeReprOfTuples(zipped)), _ => body.tpe.widen)
    )

    val (pattern, binds) = unapplies(zipped, defdefSymbol)

    val defdefStatements = DefDef(
      defdefSymbol,
      { case List(List(x)) =>
          Some {
            Match(
              x.asExpr.asTerm,
              List(
                CaseDef(
                  pattern,
                  None,
                  body.alphaRename(binds).changeOwner(defdefSymbol)
                )
              )
            )
          }
        case _ => throwGenericError()
      }
    )
    val closure = Closure(Ref(defdefSymbol), None)
    Block(List(defdefStatements), closure)
  }

  def throwGenericError(): Nothing =
    report.errorAndAbort("Oopsie, wrong argument passed to ado!")

// Block(List(DefDef(_, List(TermParamClause(List(valdef))), _, Some(rest))), _)

  //TODO(kπ) maybe should be more generic
  private def extractBodyAndValDef(expr: Term): Option[(ValDef, Term)] = expr match {
    case Block(List(DefDef(_, List(TermParamClause(List(valdef))), _, Some(rest))), _) =>
      Some(adaptValDefAndBody(valdef, rest))
    case Block(List(), expr) =>
      extractBodyAndValDef(expr)
    case _ =>
      throwGenericError()
  }

  //TODO(kπ) this can probably give false positives
  private def adaptValDefAndBody(valdef: ValDef, body: Term): (ValDef, Term) = body match {
    case Match(Typed(Ident(name), tpt), List(CaseDef(Ident(name1), _, term))) if name == valdef.name && name1 == "_" =>
      ValDef.copy(valdef)(name = "_", tpt = valdef.tpt, rhs = valdef.rhs) -> term
    case _ =>
      valdef -> body
  }

  private def supportedMethodInChain(name: String): Boolean =
    List("flatMap", "map").contains(name)

  //TODO(kπ) maybe should be more generic
  private def toBindings(exprTerm: Term, acc: List[Binding] = List.empty): (List[Binding], Term) = exprTerm match
    case Apply(Apply(TypeApply(Select(expr, methodName), typeArgs), List(arg)), args)
    if supportedMethodInChain(methodName) =>
      extractBodyAndValDef(arg) match {
        case Some((valdef, body)) =>
          toBindings(body, Binding(valdef, expr, methodName, typeArgs.map(_.tpe), args) :: acc)
        case _ =>
          acc.reverse -> exprTerm
      }
    case Apply(TypeApply(Select(expr, methodName), typeArgs), List(arg))
    if supportedMethodInChain(methodName) =>
      extractBodyAndValDef(arg) match {
        case Some((valdef, body)) =>
          toBindings(body, Binding(valdef, expr, methodName, typeArgs.map(_.tpe), List.empty) :: acc)
        case _ =>
          acc.reverse -> exprTerm
      }
    case Block(List(), exprTerm) =>
      toBindings(exprTerm, acc)
    case _ =>
      acc.reverse -> exprTerm

  extension [T <: Tree](tree: T)
    private def alphaRename(renames: Map[Symbol, Symbol]): T = {
      object treeMap extends TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
          case ident: Ident if renames.contains(ident.symbol) =>
            Ident(renames(ident.symbol).termRef)
          case _ =>
            super.transformTerm(tree)(owner)
        }
      treeMap.transformTree(tree)(Symbol.spliceOwner).asInstanceOf[T]
    }

  extension (term: Term)
    private def appliedToArgsIfNeeded(args: List[Term]): Term = {
      if args.isEmpty then
        term
      else
        term.appliedToArgs(args)
    }
  
  extension (tpe: TypeRepr)
    private def doWhatYouCanHACK: TypeRepr = tpe match {
      case AndType(lo, hi) => lo
      case OrType(lo, hi) => lo
      case _ => tpe
    }

  private def getBindingDependencies(tree: Tree, bindingVals: Set[ValDef] = Set.empty): Set[Symbol] = {
    object accumulator extends TreeAccumulator[Set[Symbol]] {
      def foldTree(acc: Set[Symbol], tree: Tree)(owner: Symbol): Set[Symbol] = tree match {
        case ident: Ident => acc + ident.symbol
        case _ => foldOverTree(acc, tree)(owner)
      }
    }
    accumulator.foldTree(Set.empty, tree)(Symbol.spliceOwner)
      .intersect(bindingVals.map(_.symbol))
  }

}