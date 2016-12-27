package org.jetbrains.plugins.scala.lang.psi.implicits

import com.intellij.psi.util.PsiTreeUtil.{findCommonContext, isContextAncestor}
import com.intellij.psi.{PsiElement, ResolveState, StubBasedPsiElement}
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
          isEligible(f, getPlace) && !isConformsMethod(f)

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

  def isEligible(function: ScFunction, expression: PsiElement): Boolean = {
    Some(function)
      .filter(!_.hasExplicitType)
      .filter(f => isContextAncestor(f.getContainingFile, expression, false))
      .map(findCommonContext(_, expression))
      .forall {
        case context if context == expression => true // weird case, it covers situation, when function comes from object, not treeWalkUp
        case context if context == function => false
        case context =>
          implicit val commonContext = context
          deepSameElementInContext(function)
            .zip(deepSameElementInContext(expression))
            .forall {
              case (functionElement, expressionElement) => isDefinedEarlier(functionElement, expressionElement)
            }
      }
  }

  private def deepSameElementInContext(element: PsiElement)
                                      (implicit context: PsiElement): Option[PsiElement] = {
    var maybeContext = Option(element.getContext)
    while (maybeContext.isDefined && maybeContext.get != context) {
      maybeContext = maybeContext.map(_.getContext)
    }

    maybeContext.collect {
      case scalaElement: ScalaPsiElement => scalaElement
    }.map(_.getDeepSameElementInContext)
  }

  private def children(implicit context: PsiElement): Seq[PsiElement] = context match {
    case stub: StubBasedPsiElement[_] if stub.getStub != null =>
      import scala.collection.JavaConversions._
      stub.getStub.getChildrenStubs.map(_.getPsi)
    case _ => context.getChildren
  }

  private def isDefinedEarlier(functionElement: PsiElement, expressionElement: PsiElement)
                              (implicit context: PsiElement): Boolean =
    children.find { child =>
      child == functionElement || child == expressionElement
    }.forall(_ != expressionElement)
}

