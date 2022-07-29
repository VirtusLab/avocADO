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

    val bindingsWithDependencies: List[(Binding, Set[String])] = bindings.map {
      case binding => (binding, getBindingDependencies(binding.tree, bindingVals))
    }

    connectBindings(bindingsWithDependencies, res).asExprOf[F[A]]
  }

  private def connectBindings(bindings: List[(Binding, Set[String])], res: Term): Tree = {

    def go(bindings: List[(Binding, Set[String])], zipped: List[ValDef], acc: Term): Term = bindings match {
      case Nil =>
        val term: Select = acc.select(acc.tpe.typeSymbol.methodMember("map").head)
        term.appliedToType(res.tpe).appliedTo(funForZipped(zipped, res, Symbol.spliceOwner))
      case _ =>
        val term: Select = acc.select(acc.tpe.typeSymbol.methodMember("flatMap").head)
        val (toZip, rest) = splitToZip(bindings)
        val body = go(rest, toZip.map(_._1.valdef), zipExprs(toZip))
        val tpe = extractTypeFromApplicative(body.tpe)
        term.appliedToType(tpe).appliedTo(funForZipped(zipped, body, Symbol.spliceOwner))
    }

    val (toZip, rest) = splitToZip(bindings)
    val r = go(rest, toZip.map(_._1.valdef), zipExprs(toZip))
    r
  }

  private def extractTypeFromApplicative(typeRepr: TypeRepr): TypeRepr = typeRepr match {
    case AppliedType(_, List(tpe)) => tpe
  }

  private def zipExprs(toZip: List[(Binding, Set[String])]): Term = {
    toZip.tail.foldRight(toZip.head._1.tree) {
      case ((binding, deps), acc) =>
        val term: Select = binding.tree.select(binding.tree.tpe.typeSymbol.methodMember("zip").head)
        val tpe = extractTypeFromApplicative(acc.tpe)
        term.appliedToTypes(List(binding.valdef.tpt.tpe, tpe)).appliedTo(acc)
    }
  }

  private def splitToZip(bindings: List[(Binding, Set[String])]): (List[(Binding, Set[String])], List[(Binding, Set[String])]) = {
    @tailrec
    def go(
      toZip: List[(Binding, Set[String])],
      dropped: List[(Binding, Set[String])],
      bindings: List[(Binding, Set[String])]
    ): (List[(Binding, Set[String])], List[(Binding, Set[String])]) = bindings match {
      case Nil =>
        (toZip, dropped)
      case head :: bindings =>
        val (take, drop) = bindings.span(!_._2.contains(head._1.valdef.name))
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

    def unapplies(zipped: List[ValDef], owner: Symbol): (Tree, Map[String, Symbol]) = zipped match {
      case Nil =>
        throwGenericError()
      case head :: Nil =>
        val sym = Symbol.newBind(owner, head.name, Flags.EmptyFlags, head.tpt.tpe)
        Bind(sym, Wildcard()) -> Map(head.name -> sym)
      case head :: zipped =>
        val sym = Symbol.newBind(owner, head.name, Flags.EmptyFlags, head.tpt.tpe)
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
            Bind(sym, Wildcard()),
            tree
          )
        ) -> (binds + (head.name -> sym))
    }

    val defdefSymbol = Symbol.newMethod(
      owner,
      "$anonfun",
      MethodType(List("syth$x$"))(_ => List(typeReprOfTuples(zipped)), _ => body.tpe)
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
                  body.alphaRename(binds)
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

  private def toBindings(exprTerm: Term, acc: List[Binding] = List.empty): (List[Binding], Term) = exprTerm match
    case Apply(TypeApply(Select(expr, "flatMap"), _), List(Block(List(DefDef(_, List(TermParamClause(List(valdef))), _, Some(rest))), _))) =>
      toBindings(rest, Binding(valdef, expr) :: acc)
    case Apply(TypeApply(Select(expr, "map"), _), List(Block(List(DefDef(_, List(TermParamClause(List(valdef))), _, Some(rest))), _))) =>
      (Binding(valdef, expr) :: acc).reverse -> rest

  //TODO(kπ) This can also lead to false positives
  extension [T <: Tree](tree: T) private def alphaRename(renames: Map[String, Symbol]): T = {
    object treeMap extends TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case Ident(name) if renames.contains(name) =>
          Ident(renames(name).termRef)
        case _ =>
          super.transformTerm(tree)(owner)
      }
    treeMap.transformTree(tree)(tree.symbol.owner).asInstanceOf[T]
  }

  //TODO(kπ) This can have false positives (non free variables that shadow binding variables) good enough for now
  private def getBindingDependencies(tree: Tree, bindingVals: Set[ValDef] = Set.empty): Set[String] = {
    object accumulator extends TreeAccumulator[Set[String]] {
      def foldTree(acc: Set[String], tree: Tree)(owner: Symbol): Set[String] = tree match {
        case Ident(name) => acc + name
        case _ => foldOverTree(acc, tree)(owner)
      }
    }
    accumulator.foldTree(Set.empty, tree)(tree.symbol.owner)
      .intersect(bindingVals.map(_.name))
  }

}