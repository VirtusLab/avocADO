package avocado

import scala.annotation.tailrec
import scala.quoted.*

object macros {

  def adoImpl[F[_]: Type, A: Type](compExpr: Expr[F[A]], apExpr: Expr[AvocADO[F]])(using Quotes): Expr[F[A]] =
    ADOImpl(using quotes).adoImpl(compExpr, apExpr)

}

class ADOImpl(using Quotes) {
  import quotes.reflect.*

  private case class VarRef(
    symbol: Symbol,
    tpt: TypeTree,
    name: String
  )

  private case class Binding(
    varrefs: List[VarRef],
    tree: Term,
    methodName: String,
    typeArgs: List[TypeRepr],
    additionalArgs: List[Term]
  )

  def adoImpl[F[_]: Type, A: Type](compExpr: Expr[F[A]], apExpr: Expr[AvocADO[F]])(using Quotes): Expr[F[A]] = {
    val exprTree = compExpr.asTerm match
      case Inlined(_, _, tree) => tree match
        case Block(Nil, expr) => expr
        case expr => expr
      case _ => throwGenericError()

    val (bindings, res) = toBindings(exprTree)

    val bindingVals: Set[VarRef] = bindings.flatMap(_.varrefs).toSet

    val bindingsWithDependencies: List[(Binding, Set[Symbol])] = bindings.map {
      case binding => (binding, getBindingDependencies(binding.tree, bindingVals))
    }

    connectBindings(bindingsWithDependencies, res, apExpr.asTerm).asExprOf[F[A]]
  }

  private def connectBindings(bindings: List[(Binding, Set[Symbol])], res: Term, ap: Term): Tree = {

    def go(bindings: List[(Binding, Set[Symbol])], zipped: List[List[VarRef]], acc: Term, lastBinding: Binding): Term = bindings match {
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
        val body = go(rest, toZip.map(_._1.varrefs), zipExprs(toZip, ap), newLastBinding)
        val tpes = lastBinding.typeArgs.map(_.widen)
        term
          .appliedToTypes(tpes)
          .appliedTo(funForZipped(zipped, body, Symbol.spliceOwner))
          .appliedToArgsIfNeeded(lastBinding.additionalArgs)
    }

    val (toZip, rest, lastMethod) = splitToZip(bindings)
    go(rest, toZip.map(_._1.varrefs), zipExprs(toZip, ap), lastMethod)
  }

  private def extractTypeFromApplicative(typeRepr: TypeRepr): TypeRepr = typeRepr.widen match {
    case AppliedType(_, args) => args.last
  }

  private def doZip(receiver: Term, varrefs: List[VarRef], arg: Term, ap: Term): Term = {
    val receiverTypeSymbol = receiver.tpe.typeSymbol
    val argTpe = extractTypeFromApplicative(arg.tpe)
    ap
      .select(ap.tpe.typeSymbol.methodMember("zip").head)
      .appliedToTypes(List(typeReprFromVarRefs(varrefs).widen, argTpe.widen)).appliedTo(receiver, arg)
  }

  private def zipExprs(toZip: List[(Binding, Set[Symbol])], ap: Term): Term = {
    toZip.init.foldRight(toZip.last._1.tree) {
      case ((binding, _), acc) =>
        doZip(binding.tree, binding.varrefs, acc, ap)
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
        val (take, drop) = bindings.span( b => b._2.intersect(head._1.varrefs.map(_.symbol).toSet).isEmpty)
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

  private def typeReprFromVarRefs(varrefs: List[VarRef]): TypeRepr = varrefs match {
    case List(varref) =>
      varref.tpt.tpe
    case _ =>
      val tupleSym = defn.TupleClass(varrefs.size)
      AppliedType(tupleSym.typeRef, varrefs.map(_.tpt.tpe))
  }

  private def funForZipped(zipped0: List[List[VarRef]], body: Term, owner: Symbol): Term = {
    val tuple2: Term = '{Tuple2}.asTerm.asInstanceOf[Inlined].body
    val AppliedType(tuple2Tpe: TypeRepr, _) = TypeRepr.of[Tuple2[Any, Any]]: @unchecked

    def shadowDuplicates(zipped: List[List[VarRef]]): List[List[VarRef]] = {
      zipped.foldRight(List.empty[List[VarRef]] -> Set.empty) {
        case (elem, (acc, prevs)) =>
          val (accElem, prevs1) = elem.foldRight(List.empty[VarRef] -> prevs) {
            case (e, (accElem, prev1)) =>
              if prev1.contains(e.name) then
                (e.copy(name = "_") +: accElem) -> prev1
              else
                (e +: accElem) -> (prev1 + e.name)
          }
          (accElem +: acc) -> prevs1
      }._1
    }

    def typeReprOfTuples(zipped: List[List[VarRef]]): TypeRepr = zipped match {
      case Nil =>
        throwGenericError()
      case head :: Nil =>
        typeReprFromVarRefs(head)
      case head :: zipped =>
        AppliedType(
          tuple2Tpe,
          List(
            typeReprFromVarRefs(head),
            typeReprOfTuples(zipped)
          )
        )
    }

    def makeBind(varref: VarRef, owner: Symbol): (Tree, Map[Symbol, Symbol]) = {
      if varref.name == "_" then
        Wildcard() -> Map.empty
      else
        val sym = Symbol.newBind(owner, varref.name, Flags.EmptyFlags, varref.tpt.tpe)
        Bind(sym, Wildcard()) -> Map(varref.symbol -> sym)
    }

    def makeUnapplies(varrefs: List[VarRef], owner: Symbol): (Tree, Map[Symbol, Symbol]) = varrefs match {
      case varref :: Nil =>
        makeBind(varref, owner)
      case _ =>
        val (binds, renames) = varrefs.map(makeBind(_, owner)).foldLeft(List.empty[Tree] -> Map.empty[Symbol, Symbol]) {
          case ((accBinds, accRenames), (bind, renames)) =>
            (accBinds :+ bind) -> (accRenames ++ renames)
        }
        val tupleModuleSym = defn.TupleClass(varrefs.size).companionModule
        Unapply(
          TypeApply(
            Ref(tupleModuleSym).select(tupleModuleSym.methodMember("unapply").head),
            varrefs.map(_.tpt)
          ),
          List.empty,
          binds
        ) -> renames
    }

    def unapplies(zipped: List[List[VarRef]], owner: Symbol): (Tree, Map[Symbol, Symbol]) = zipped match {
      case Nil =>
        throwGenericError()
      case head :: Nil =>
        makeUnapplies(head, owner)
      case head :: zipped =>
        val (bind, renames) = makeUnapplies(head, owner)
        val (tree, binds) = unapplies(zipped, owner)
        Unapply(
          TypeApply(
            Select(tuple2, tuple2.tpe.typeSymbol.methodMember("unapply").head),
            List(
              Inferred(typeReprFromVarRefs(head)),
              Inferred(typeReprOfTuples(zipped))
            )
          ),
          List.empty,
          List(
            bind,
            tree
          )
        ) -> (binds ++ renames)
    }

    val zipped = shadowDuplicates(zipped0)

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
              Typed(x.asExpr.asTerm, Inferred(AnnotatedType(x.asExpr.asTerm.tpe, New(Inferred(Symbol.requiredClass("scala.unchecked").typeRef))))),
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

  //TODO(kπ) maybe should be more generic
  private def extractBodyAndVarRefs(expr: Term): Option[(List[VarRef], Term)] = expr match {
    case Block(List(DefDef(_, List(TermParamClause(List(valdef))), _, Some(rest))), _) =>
      Some(adaptValDefAndBody(valdef, rest))
    case Block(List(), expr) =>
      extractBodyAndVarRefs(expr)
    case _ =>
      throwGenericError()
  }

  //TODO(kπ) this can probably give false positives
  private def adaptValDefAndBody(valdef: ValDef, body: Term): (List[VarRef], Term) = body match {
    case Match(Typed(Ident(name), tpt), List(CaseDef(ident@Ident(name1), _, term))) if name == valdef.name && name1 == "_" =>
      List(VarRef(ident.symbol, tpt, name1)) -> term
    case Match(Typed(Ident(name), _), List(CaseDef(Unapply(TypeApply(Select(prefix, method), tpts), _, patterns), _, term))) if defn.isTupleClass(prefix.symbol.companionClass) && method == "unapply" =>
      patterns.zip(tpts).map {
        case (bind@Bind(name, _), tpt) =>
          VarRef(bind.symbol, tpt, name)
      } -> term
    case _ =>
      List(VarRef(valdef.symbol, valdef.tpt, valdef.name)) -> body
  }

  private def supportedMethodInChain(name: String): Boolean =
    List("flatMap", "map").contains(name)

  //TODO(kπ) maybe should be more generic
  private def toBindings(exprTerm: Term, acc: List[Binding] = List.empty): (List[Binding], Term) = exprTerm match
    case Apply(Apply(TypeApply(Select(expr, methodName), typeArgs), List(arg)), args)
    if supportedMethodInChain(methodName) =>
      extractBodyAndVarRefs(arg) match {
        case Some((varrefs, body)) =>
          toBindings(body, Binding(varrefs, expr, methodName, typeArgs.map(_.tpe), args) :: acc)
        case _ =>
          acc.reverse -> exprTerm
      }
    case Apply(TypeApply(Select(expr, methodName), typeArgs), List(arg))
    if supportedMethodInChain(methodName) =>
      extractBodyAndVarRefs(arg) match {
        case Some((varrefs, body)) =>
          toBindings(body, Binding(varrefs, expr, methodName, typeArgs.map(_.tpe), List.empty) :: acc)
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

  private def getBindingDependencies(tree: Tree, bindingVals: Set[VarRef] = Set.empty): Set[Symbol] = {
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