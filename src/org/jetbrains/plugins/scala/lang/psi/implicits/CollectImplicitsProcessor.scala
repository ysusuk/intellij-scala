package org.jetbrains.plugins.scala.lang.psi.implicits

import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScBindingPattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScObject, ScTrait}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.types.ScalaType.designator
import org.jetbrains.plugins.scala.lang.psi.types.api.{TypeParameterType, TypeSystem, UndefinedType}
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScSubstitutor, ScType, ScalaType}
import org.jetbrains.plugins.scala.lang.psi.{ScalaPsiElement, ScalaPsiUtil}
import org.jetbrains.plugins.scala.lang.resolve.ResolveUtils.isAccessible
import org.jetbrains.plugins.scala.lang.resolve.processor.{BaseProcessor, ImplicitProcessor}
import org.jetbrains.plugins.scala.lang.resolve.{ScalaResolveResult, StdKinds}

/**
  * @author adkozlov
  */
class CollectImplicitsProcessor(val getPlace: ScExpression, withoutPrecedence: Boolean)
                               (implicit override val typeSystem: TypeSystem)
  extends ImplicitProcessor(StdKinds.refExprLastRef, withoutPrecedence) {

  def execute(element: PsiElement, state: ResolveState): Boolean = {
    val function1Type = getPlace.elementScope.function1Type(level = 0).getOrElse {
      return true
    }

    import CollectImplicitsProcessor._

    def isAccessibleImplicit(member: ScMember) =
      member.hasModifierProperty("implicit") && isAccessible(member, getPlace)

    val maybeDefinition = Option(element).collect {
      case definition: ScTypedDefinition => definition
    }.filter(kindMatches).filter {
      //there is special case for Predef.conforms method
      case f: ScFunction =>
        val functionIsAppropriate = isAccessibleImplicit(f) &&
          checkFucntionIsEligible(f, getPlace) && !isConformsMethod(f)

        val clauseIsAppropriate = f.paramClauses.clauses match {
          case Seq(_, _, _, _*) => false
          case Seq(_, clause) => clause.isImplicit
          case Seq(clause, _*) => !clause.isImplicit && clause.parameters.length == 1
          case _ => true
        }

        functionIsAppropriate && clauseIsAppropriate
      case b: ScBindingPattern =>
        ScalaPsiUtil.nameContext(b) match {
          case context: ScValueOrVariable => isAccessibleImplicit(context)
          case _ => false
        }
      case p: ScClassParameter => p.isImplicitParameter && isAccessible(p, getPlace)
      case p: ScParameter => p.isImplicitParameter
      case o: ScObject => isAccessibleImplicit(o)
      case _ => false
    }

    def substitute(`type`: ScType): (ScType, ScSubstitutor) = {
      val substitutor = getSubst(state)
      val actualSubstitutor = state.get(BaseProcessor.FROM_TYPE_KEY) match {
        case null => substitutor
        case tp => substitutor.followUpdateThisType(tp)
      }

      (actualSubstitutor.subst(`type`), actualSubstitutor)
    }

    val maybeSubstitutor = maybeDefinition.flatMap(_.getType().toOption)
      .map(substitute)
      .filter {
        case (tp, _) => tp.conforms(function1Type)
      }.map(_._2)

    maybeDefinition.zip(maybeSubstitutor).map {
      case (definition, substitutor) => new ScalaResolveResult(definition, substitutor, getImports(state))
    }.foreach(addResult)

    true
  }
}


object CollectImplicitsProcessor {
  private def isConformsMethod(f: ScFunction): Boolean = {
    f.name.matches("(\\$)?conforms") &&
      Option(f.containingClass).flatMap { clazz =>
        Option(clazz.qualifiedName)
      }.contains("scala.Predef")
  }

  def checkFucntionIsEligible(function: ScFunction, expression: PsiElement): Boolean = {
    if (!function.hasExplicitType) {
      if (PsiTreeUtil.isContextAncestor(function.getContainingFile, expression, false)) {
        val commonContext = PsiTreeUtil.findCommonContext(function, expression)
        if (expression == commonContext) return true //weird case, it covers situation, when function comes from object, not treeWalkUp
        if (function == commonContext) return false
        else {
          var functionContext: PsiElement = function
          while (functionContext.getContext != commonContext) functionContext = functionContext.getContext
          var placeContext: PsiElement = expression
          while (placeContext.getContext != commonContext) placeContext = placeContext.getContext
          (functionContext, placeContext) match {
            case (functionContext: ScalaPsiElement, placeContext: ScalaPsiElement) =>
              val funElem = functionContext.getDeepSameElementInContext
              val conElem = placeContext.getDeepSameElementInContext
              val children = commonContext match {
                case stubPsi: StubBasedPsiElement[_] =>
                  val stub = stubPsi.getStub
                  import scala.collection.JavaConverters._
                  if (stub != null) stub.getChildrenStubs.asScala.map(_.getPsi).toArray
                  else stubPsi.getChildren
                case _ => commonContext.getChildren
              }
              children.find(elem => elem == funElem || elem == conElem) match {
                case Some(elem) if elem == conElem => return false
                case _ =>
              }
            case _ =>
          }
        }
      }
    }
    true
  }
}

