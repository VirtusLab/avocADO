package avocado

import scala.annotation.tailrec
import scala.quoted.*

object macros {

  def adoImpl[F[_]: Type, A: Type](compExpr: Expr[F[A]])(using Quotes): Expr[F[A]] =
    ADOImpl(using quotes).adoImpl(compExpr)

}

class ADOImpl(using Quotes) {
  import quotes.reflect.*

  private case class Binding(
    valdef: ValDef,
    tree: Term
  )

  def adoImpl[F[_]: Type, A: Type](compExpr: Expr[F[A]])(using Quotes): Expr[F[A]] = {
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

    connectBindings(bindingsWithDependencies, res).asExprOf[F[A]]
  }

  private def connectBindings(bindings: List[(Binding, Set[Symbol])], res: Term): Tree = {

    def go(bindings: List[(Binding, Set[Symbol])], zipped: List[ValDef], acc: Term): Term = bindings match {
      case Nil =>
        val term: Select = acc.select(acc.tpe.typeSymbol.methodMember("map").head)
        term.appliedToType(res.tpe.widen).appliedTo(funForZipped(zipped, res, Symbol.spliceOwner))
      case _ =>
        val term: Select = acc.select(acc.tpe.typeSymbol.methodMember("flatMap").head)
        val (toZip, rest) = splitToZip(bindings)
        val body = go(rest, toZip.map(_._1.valdef), zipExprs(toZip))
        val tpe = extractTypeFromApplicative(body.tpe)
        term.appliedToType(tpe.widen).appliedTo(funForZipped(zipped, body, Symbol.spliceOwner))
    }

    val (toZip, rest) = splitToZip(bindings)
    go(rest, toZip.map(_._1.valdef), zipExprs(toZip))
  }

  private def extractTypeFromApplicative(typeRepr: TypeRepr): TypeRepr = typeRepr.widen match {
    case AppliedType(_, args) => args.last
  }

  private def doZip(receiver: Term, valdef: ValDef, arg: Term): Term = {
    val receiverTypeSymbol = receiver.tpe.typeSymbol
    val argTpe = extractTypeFromApplicative(arg.tpe)
    if receiverTypeSymbol.methodMember("zip").nonEmpty then
      val term: Select = receiver.select(receiverTypeSymbol.methodMember("zip").head)
      term.appliedToTypes(List(valdef.tpt.tpe.widen, argTpe.widen)).appliedTo(arg)
    else if receiverTypeSymbol.methodMember("both").nonEmpty then
      val term: Select = receiver.select(receiverTypeSymbol.methodMember("both").head)
      term.appliedToTypes(List(argTpe.widen)).appliedTo(arg)
    else
      throwGenericError()
  }

  private def zipExprs(toZip: List[(Binding, Set[Symbol])]): Term = {
    toZip.init.foldRight(toZip.last._1.tree) {
      case ((binding, _), acc) =>
        doZip(binding.tree, binding.valdef, acc)
    }
  }

  private def splitToZip(bindings: List[(Binding, Set[Symbol])]): (List[(Binding, Set[Symbol])], List[(Binding, Set[Symbol])]) = {
    @tailrec
    def go(
      toZip: List[(Binding, Set[Symbol])],
      dropped: List[(Binding, Set[Symbol])],
      bindings: List[(Binding, Set[Symbol])]
    ): (List[(Binding, Set[Symbol])], List[(Binding, Set[Symbol])]) = bindings match {
      case Nil =>
        (toZip, dropped)
      case head :: bindings =>
        val (take, drop) = bindings.span(!_._2.contains(head._1.valdef.symbol))
        go(toZip :+ head, drop ++ dropped, take)
    }

    go(List.empty, List.empty, bindings)
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

  //TODO(kπ) this can probably give false positives
  private def extractBodyAndValDef(body: Term, valdef: ValDef): (Term, ValDef) = body match {
    case Match(Typed(Ident(name), tpt), List(CaseDef(Ident(name1), _, term))) if name == valdef.name && name1 == "_" =>
      term -> ValDef.copy(valdef)(name = "_", tpt = valdef.tpt, rhs = valdef.rhs)
    case _ =>
      body -> valdef
  }

  private def toBindings(exprTerm: Term, acc: List[Binding] = List.empty): (List[Binding], Term) = exprTerm match
    case Apply(TypeApply(Select(expr, "flatMap"), _), List(Block(List(DefDef(_, List(TermParamClause(List(valdef))), _, Some(rest))), _))) =>
      val (body, vd) = extractBodyAndValDef(rest, valdef)
      toBindings(body, Binding(vd, expr) :: acc)
    case Apply(TypeApply(Select(expr, "map"), _), List(Block(List(DefDef(_, List(TermParamClause(List(valdef))), _, Some(rest))), _))) =>
      val (body, vd) = extractBodyAndValDef(rest, valdef)
      (Binding(vd, expr) :: acc).reverse -> body

  extension [T <: Tree](tree: T) private def alphaRename(renames: Map[Symbol, Symbol]): T = {
    object treeMap extends TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case ident: Ident if renames.contains(ident.symbol) =>
          Ident(renames(ident.symbol).termRef)
        case _ =>
          super.transformTerm(tree)(owner)
      }
    treeMap.transformTree(tree)(Symbol.spliceOwner).asInstanceOf[T]
  }

  private def getBindingDependencies(tree: Tree, bindingVals: Set[ValDef] = Set.empty): Set[Symbol] = {
    object accumulator extends TreeAccumulator[Set[Symbol]] {
      def foldTree(acc: Set[Symbol], tree: Tree)(owner: Symbol): Set[Symbol] = tree match {
        case ident: Ident => acc + ident.symbol
        case _ => foldOverTree(acc, tree)(owner)
      }
    }
    accumulator.foldTree(Set.empty, tree)(tree.symbol.owner)
      .intersect(bindingVals.map(_.symbol))
  }

}