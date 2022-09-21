package avocado

import scala.annotation.tailrec
import scala.collection.mutable
import scala.quoted.*

private[avocado] object macros {

  def adoImpl[F[_]: Type, A: Type](compExpr: Expr[F[A]], instanceExpr: Expr[AvocADO[F]])(using Quotes): Expr[F[A]] =
    ADOImpl(using quotes).adoImpl(compExpr, instanceExpr)

  class ADOImpl(using Quotes) {
    import quotes.reflect.*

    private case class VarRef(
      symbol: Symbol,
      name: String
    )

    private case class Binding(
      varrefs: List[VarRef],
      pattern: Tree,
      tpe: TypeRepr,
      tree: Term,
      methodName: String,
      typeArgs: List[TypeRepr],
      additionalArgs: List[Term]
    )

    private case class Context(
      instance: Term,
      fTpe: TypeRepr
    )

    private def ctx(using context: Context): Context = context

    def adoImpl[F[_]: Type, A: Type](compExpr: Expr[F[A]], instanceExpr: Expr[AvocADO[F]])(using Quotes): Expr[F[A]] = {
      val exprTree = compExpr.asTerm match
        case Inlined(_, _, tree) => tree match
          case Block(Nil, expr) => expr
          case expr => expr
        case _ => throwGenericError()

      given Context = Context(instanceExpr.asTerm, TypeRepr.of[F])

      val (bindings, res) = toBindings(exprTree)

      val bindingVals: Set[VarRef] = bindings.flatMap(_.varrefs).toSet

      val bindingsWithDependencies: List[(Binding, Set[Symbol])] = bindings.map {
        case binding => (binding, getBindingDependencies(binding.tree, bindingVals))
      }

      connectBindings(bindingsWithDependencies, res).asExprOf[F[A]]
    }

    private def connectBindings(bindings: List[(Binding, Set[Symbol])], res: Term)(using Context): Tree = {
      def go(bindings: List[(Binding, Set[Symbol])], zipped: List[(Tree, TypeRepr)], acc: Term, lastBinding: Binding): Term = bindings match {
        case Nil =>
          val arg = funFromZipped(zipped, res, Symbol.spliceOwner)
          ctx.instance
            .select(ctx.instance.tpe.typeSymbol.methodMember(lastBinding.methodName).head)
            .appliedToTypes(List(typeReprForBindings(zipped), adaptTpeForMethod(res, lastBinding.methodName)))
            .appliedToArgs(List(acc, arg))
        case _ =>
          val (toZip, rest, newLastBinding) = splitToZip(bindings)
          val body = go(rest, toZip.map(b => b._1.pattern -> b._1.tpe), zipExprs(toZip.map(_._1), Symbol.spliceOwner), newLastBinding)
          val arg = funFromZipped(zipped, body, Symbol.spliceOwner)
          val tpes = lastBinding.typeArgs.map(_.widen)
          ctx.instance
            .select(ctx.instance.tpe.typeSymbol.methodMember(lastBinding.methodName).head)
            .appliedToTypes(List(typeReprForBindings(zipped), adaptTpeForMethod(body, lastBinding.methodName)))
            .appliedToArgs(List(acc, arg))
      }

      val (toZip, rest, lastMethod) = splitToZip(bindings)
      go(rest, toZip.map(b => b._1.pattern -> b._1.tpe), zipExprs(toZip.map(_._1), Symbol.spliceOwner), lastMethod)
    }

    private def adaptTpeForMethod(arg: Term, methodName: String): TypeRepr =
      methodName match {
        case "map" => arg.tpe.widen
        case "flatMap" => extractTypeFromApplicative(arg.tpe.widen)
      }

    private def extractTypeFromApplicative(typeRepr: TypeRepr): TypeRepr = typeRepr.widen match {
      case AppliedType(_, args) => args.last
    }

    private def zipExprs(toZip: List[Binding], owner: Symbol)(using Context): Term = {
      def doZip(receiver: Term, receiverTpe: TypeRepr, arg: Term)(using Context): Term = {
        val receiverTypeSymbol = receiver.tpe.typeSymbol
        val argTpe = extractTypeFromApplicative(arg.tpe)
        ctx.instance
          .select(ctx.instance.tpe.typeSymbol.methodMember("zip").head)
          .appliedToTypes(List(receiverTpe.widen, argTpe.widen))
          .appliedTo(receiver, arg)
      }
      
      toZip.init.foldRight(toZip.last.tree) {
        case (binding, acc) =>
          doZip(binding.tree, binding.tpe, acc)
      }.changeOwner(owner)
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

    private val tuple2: Term = Ref(Symbol.requiredModule("scala.Tuple2"))
    private val tuple2Tpe: TypeRepr = Symbol.requiredClass("scala.Tuple2").typeRef

    private def typeReprForBindings(zipped: List[(Tree, TypeRepr)]): TypeRepr = zipped match {
      case Nil =>
        throwGenericError()
      case (_, tpe) :: Nil =>
        tpe
      case (_, tpe) :: zipped =>
        AppliedType(tuple2Tpe, List(tpe, typeReprForBindings(zipped)))
    }

    private def funFromZipped(zipped: List[(Tree, TypeRepr)], body: Term, owner: Symbol): Term = {

      def makeUnapplies(unaply: Tree, owner: Symbol, binds0: Set[String]): (Tree, Map[Symbol, Symbol]) = {
        val renamesRes = mutable.Map.empty[Symbol, Symbol] // sorry :/
        def binds = renamesRes.values.map(_.name).toSet ++ binds0
        object mapper extends TreeMap {
          override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
            case Ident(name) if name == "_" =>
              Wildcard()
            case _ =>
              super.transformTerm(tree)(owner)
          }
          override def transformTree(tree: Tree)(owner: Symbol): Tree = tree match {
            case valdef: ValDef if valdef.name == "_" || binds.contains(valdef.name) =>
              Wildcard()
            case valdef: ValDef =>
              val sym = Symbol.newBind(owner, valdef.name, Flags.EmptyFlags, valdef.tpt.tpe)
              renamesRes += (valdef.symbol -> sym)
              Bind(sym, Wildcard())
            case bind@Bind(name, pattern0) =>
              val sym = Symbol.newBind(owner, name, Flags.EmptyFlags, bind.symbol.typeRef.widen)
              renamesRes += (bind.symbol -> sym)
              Bind(sym, transformTree(pattern0)(owner))
            case Typed(term, _) =>
              transformTerm(term)(owner)
            case unaply@Unapply(fun, implicits, patterns0) =>
              val patterns =
                patterns0.foldRight(List.empty[Tree]) {
                  case (p, accList) =>
                    val pattern = transformTree(p)(owner)
                    pattern :: accList
                }
              Unapply.copy(unaply)(fun, implicits, patterns).changeOwner(owner)
            case _ =>
              super.transformTree(tree)(owner).changeOwner(owner)
          }
        }
        mapper.transformTree(unaply)(owner) -> renamesRes.to(Map)
      }

      def unapplies(zipped: List[(Tree, TypeRepr)], owner: Symbol): (Tree, Map[Symbol, Symbol]) = zipped match {
        case Nil =>
          throwGenericError()
        case (pattern, tpe) :: Nil =>
          makeUnapplies(pattern, owner, Set.empty)
        case (pattern0, tpe) :: zipped =>
          val (restPattern, restRenames) = unapplies(zipped, owner)
          val (pattern, renames) = makeUnapplies(pattern0, owner, restRenames.keys.map(_.name).toSet)
          Unapply(
            fun = TypeApply(
              Select(tuple2, tuple2.tpe.typeSymbol.methodMember("unapply").head),
              List(
                Inferred(tpe),
                Inferred(typeReprForBindings(zipped))
              )
            ),
            implicits = List.empty,
            patterns = List(
              pattern,
              restPattern
            )
          ) -> (restRenames ++ renames)
      }

      val defdefSymbol = Symbol.newMethod(
        owner,
        "$anonfun$synth",
        MethodType(List("syth$x$"))(_ => List(typeReprForBindings(zipped)), _ => body.tpe.widen)
      )

      val (pattern, renames) = unapplies(zipped, defdefSymbol)

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
                    body.alphaRename(renames).changeOwner(defdefSymbol)
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

    private def supportedRewriteMethod(name: String): Boolean =
      List("flatMap", "map").contains(name)

    //TODO(kπ) maybe should be more generic
    private def toBindings(exprTerm: Term, acc: List[Binding] = List.empty)(using Context): (List[Binding], Term) = exprTerm match {
      case FromTypeclassAllowed(expr, evidences, methodName, typeArgs, arg) =>
        extractBodyAndVarRefs(arg) match {
          case Some((varrefs, unaply, tpt, body)) =>
            toBindings(body, Binding(varrefs, unaply, tpt, expr, methodName, typeArgs.map(_.tpe), evidences) :: acc)
          case _ =>
            acc.reverse -> exprTerm
        }
      case NormalAllowed(expr, methodName, typeArgs, arg) =>
        extractBodyAndVarRefs(arg) match {
          case Some((varrefs, unaply, tpt, body)) =>
            toBindings(body, Binding(varrefs, unaply, tpt, expr, methodName, typeArgs.map(_.tpe), List.empty) :: acc)
          case _ =>
            acc.reverse -> exprTerm
        }
      case WithImplicitsAllowed(expr, args, methodName, typeArgs, arg) =>
        extractBodyAndVarRefs(arg) match {
          case Some((varrefs, unaply, tpt, body)) =>
            toBindings(body, Binding(varrefs, unaply, tpt, expr, methodName, typeArgs.map(_.tpe), args) :: acc)
          case _ =>
            acc.reverse -> exprTerm
        }
      case Block(List(), exprTerm) =>
        toBindings(exprTerm, acc)
      case _ =>
        acc.reverse -> exprTerm
    }

    private object FromTypeclassAllowed {
      def unapply(using Context)(term: Term): Option[(Term, List[Term], String, List[TypeTree], Term)] = term match
        case Apply(TypeApply(Select(Apply(Apply(TypeApply(_, targs), List(expr)), evidences), methodName), typeArgs), List(arg))
        if supportedRewriteMethod(methodName) && targs.map(_.tpe.typeSymbol).contains(ctx.fTpe.typeSymbol) =>
          Some((expr, evidences, methodName, typeArgs, arg))
        case _ => None
    }

    private object NormalAllowed {
      def unapply(term: Term): Option[(Term, String, List[TypeTree], Term)] = term match
        case Apply(TypeApply(Select(expr, methodName), typeArgs), List(arg))
        if supportedRewriteMethod(methodName) =>
          Some((expr, methodName, typeArgs, arg))
        case _ => None
    }

    private object WithImplicitsAllowed {
      def unapply(term: Term): Option[(Term, List[Term], String, List[TypeTree], Term)] = term match
        case Apply(Apply(TypeApply(Select(expr, methodName), typeArgs), List(arg)), args)
        if supportedRewriteMethod(methodName) =>
          Some((expr, args, methodName, typeArgs, arg))
        case _ => None
    }

    //TODO(kπ) maybe should be more generic
    private def extractBodyAndVarRefs(expr: Term): Option[(List[VarRef], Tree, TypeRepr, Term)] = expr match {
      case Block(List(DefDef(_, List(TermParamClause(List(valdef))), _, Some(rest))), _) =>
        Some(adaptValDefAndBody(valdef, rest))
      case Block(List(), expr) =>
        extractBodyAndVarRefs(expr)
      case _ =>
        throwGenericError()
    }

    //TODO(kπ) this can probably give false positives
    private def adaptValDefAndBody(valdef: ValDef, body: Term): (List[VarRef], Tree, TypeRepr, Term) = body match {
      case Match(Typed(Ident(name), tpt), List(CaseDef(pattern, _, term))) if name == valdef.name =>
        (extractVarRefs(pattern), pattern, tpt.tpe.widen.dealias, term)
      case _ =>
        (List(VarRef(valdef.symbol, valdef.name)), valdef, valdef.tpt.tpe.widen.dealias, body)
    }

    private def extractVarRefs(tree: Tree): List[VarRef] = {
      object accumulator extends TreeAccumulator[List[VarRef]] {
        override def foldTree(acc: List[VarRef], tree: Tree)(owner: Symbol): List[VarRef] = tree match {
          case bind@Bind(name, pattern) =>
            foldTree(acc :+ VarRef(bind.symbol, name), pattern)(owner)
          case ident@Ident(name) =>
            acc :+ VarRef(ident.symbol, name)
          case Unapply(_, _, patterns) =>
            foldTrees(acc, patterns)(owner)
          case tree =>
            super.foldOverTree(acc, tree)(owner)
        }
      }
      accumulator.foldTree(List.empty, tree)(Symbol.spliceOwner)
    }

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
}
