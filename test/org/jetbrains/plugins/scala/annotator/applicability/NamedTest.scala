package org.jetbrains.plugins.scala
package annotator.applicability

import lang.psi.types.{ParameterSpecifiedMultipleTimes, PositionalAfterNamedArgument}

/**
 * Pavel.Fatin, 18.05.2010
 */

class NamedTest extends Base {
  def testFine {
    assertMatches(problems("def f(a: A) {}; f(a = A)")) {
      case Nil =>
    }
    assertMatches(problems("def f(a: A, b: B) {}; f(a = A, b = B)")) {
      case Nil =>
    }
  }

  def testReversed {
    assertMatches(problems("def f(a: A, b: B) {}; f(b = B, a = A)")) {
      case Nil =>
    }
  }

  def testPositionalWithNamed {
    assertMatches(problems("def f(a: A, b: B) {}; f(A, b = B)")) {
      case Nil =>
    }
    //TODO compiler allows such calls, they seem to be OK 
    //    assertMatches(problems("def f(a: A, b: B) {}; f(a = A, b)")) {
    //      case Nil =>
    //    }
  }

  def testPositionalAfterNamed {
    assertMatches(problems("def f(a: A, b: B) {}; f(b = B, A)")) {
      case PositionalAfterNamedArgument(Expression("A")) :: Nil =>
    }
    assertMatches(problems("def f(a: A, b: B, c: C) {}; f(c = C, A, B)")) {
      case PositionalAfterNamedArgument(Expression("A")) ::
              PositionalAfterNamedArgument(Expression("B")) :: Nil =>
    }
    assertMatches(problems("def f(a: A, b: B, c: C) {}; f(c = C, A, B)")) {
      case PositionalAfterNamedArgument(Expression("A")) ::
              PositionalAfterNamedArgument(Expression("B")) :: Nil =>
    }
    assertMatches(problems("def f(a: A, b: B, c: C) {}; f(A, c = C, B)")) {
      case PositionalAfterNamedArgument(Expression("B")) :: Nil =>
    }
  }

  def testNamedDuplicates {
    assertMatches(problems("def f(a: A) {}; f(a = A, a = null)")) {
      case ParameterSpecifiedMultipleTimes(Assignment("a = A")) ::
              ParameterSpecifiedMultipleTimes(Assignment("a = null")) :: Nil =>
    }
    assertMatches(problems("def f(a: A) {}; f(a = A, a = A, a = A)")) {
      case ParameterSpecifiedMultipleTimes(Assignment("a = A")) ::
              ParameterSpecifiedMultipleTimes(Assignment("a = A")) ::
              ParameterSpecifiedMultipleTimes(Assignment("a = A")) :: Nil =>
    }
    assertMatches(problems("def f(a: A, b: B) {}; f(a = A, a = null, b = B, b = null)")) {
      case ParameterSpecifiedMultipleTimes(Assignment("a = A")) ::
              ParameterSpecifiedMultipleTimes(Assignment("a = null")) ::
              ParameterSpecifiedMultipleTimes(Assignment("b = B")) ::
              ParameterSpecifiedMultipleTimes(Assignment("b = null")) :: Nil =>
    }
    assertMatches(problems("def f(a: A, b: B) {}; f(A, b = B, b = null)")) {
      case ParameterSpecifiedMultipleTimes(Assignment("b = B")) ::
              ParameterSpecifiedMultipleTimes(Assignment("b = null")) :: Nil =>
    }
  }

  // unresolved name
  // named with defaults
  // type mismatch
  // missed parameter
  // too many args
}