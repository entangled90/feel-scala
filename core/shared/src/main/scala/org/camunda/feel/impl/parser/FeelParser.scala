/*
 * Copyright Camunda Services GmbH and/or licensed to Camunda Services GmbH
 * under one or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information regarding copyright
 * ownership. Camunda licenses this file to you under the Apache License,
 * Version 2.0; you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.feel.impl.parser

import org.camunda.feel.impl.parser.FeelWhitespace.whitespace
import fastparse.{
  AnyChar,
  ByNameOps,
  ByNameOpsStr,
  CharIn,
  CharPred,
  CharsWhile,
  CharsWhileIn,
  EagerOpsStr,
  End,
  LiteralStr,
  P,
  Parsed,
  Start,
  StringIn,
  parse,
  EagerOps
}
import org.camunda.feel.syntaxtree.{
  Addition,
  ArithmeticNegation,
  AtLeastOne,
  ClosedConstRangeBoundary,
  Conjunction,
  ConstBool,
  ConstContext,
  ConstDate,
  ConstDateTime,
  ConstDayTimeDuration,
  ConstInputValue,
  ConstList,
  ConstLocalDateTime,
  ConstLocalTime,
  ConstNull,
  ConstNumber,
  ConstRange,
  ConstString,
  ConstTime,
  ConstYearMonthDuration,
  Disjunction,
  Division,
  Equal,
  EveryItem,
  Exp,
  Exponentiation,
  Filter,
  For,
  FunctionDefinition,
  FunctionInvocation,
  FunctionParameters,
  GreaterOrEqual,
  GreaterThan,
  If,
  In,
  InputEqualTo,
  InputGreaterOrEqual,
  InputGreaterThan,
  InputInRange,
  InputLessOrEqual,
  InputLessThan,
  InstanceOf,
  IterationContext,
  JavaFunctionInvocation,
  LessOrEqual,
  LessThan,
  Multiplication,
  NamedFunctionParameters,
  Not,
  OpenConstRangeBoundary,
  PathExpression,
  PositionalFunctionParameters,
  QualifiedFunctionInvocation,
  ConstRangeBoundary,
  Ref,
  SomeItem,
  Subtraction,
  UnaryTestExpression
}
import org.camunda.feel.{
  Date,
  isOffsetDateTime,
  isOffsetTime,
  isValidDate,
  isYearMonthDuration,
  isLocalDateTime,
  isDayTimeDuration,
  stringToDate,
  stringToDateTime,
  stringToDayTimeDuration,
  stringToLocalDateTime,
  stringToLocalTime,
  stringToTime,
  stringToYearMonthDuration
}

import scala.util.Try

/** The parser is written following the FEEL grammar definition in the DMN specification.
  *
  * In order to understand how the parser works, it is recommended to read the documentation first:
  * [[https://www.lihaoyi.com/fastparse]]. Additional resources:
  * [[https://www.lihaoyi.com/post/EasyParsingwithParserCombinators.html]],
  * [[https://www.lihaoyi.com/post/BuildyourownProgrammingLanguagewithScala.html]]
  */
object FeelParser {

  def parseExpression(expression: String): Parsed[Exp] =
    parse(expression, fullExpression(_))

  def parseUnaryTests(expression: String): Parsed[Exp] =
    parse(expression, fullUnaryExpression(_))

  // --------------- entry parsers ---------------

  private def fullExpression[$: P]: P[Exp] = P(Start ~ expression ~ End)

  private def fullUnaryExpression[$: P]: P[Exp] = P(Start ~ unaryTests ~ End)

  // --------------- common parsers ---------------

  // language key words that can't be used as variable names
  private def reservedWord[$: P]: P[String] =
    P(
      StringIn(
        "null",
        "true",
        "false",
        "function",
        "in",
        "return",
        "then",
        "else",
        "satisfies",
        "and",
        "or"
      )
    ).!

  // an identifier which is not a reserved word. but, it can contain a reserved word.
  private def identifier[$: P]: P[String] =
    P(
      (reservedWord ~~ namePart) | (reservedWord.? ~~ nameStart ~~ namePart.?)
    ).!

  private def nameStart[$: P]: P[String] =
    P(
      CharPred(Character.isJavaIdentifierStart)
    ).!

  private def namePart[$: P]: P[String] =
    P(
      CharsWhile(c => Character.isJavaIdentifierPart(c) && !FeelWhitespace.isWhitespace(c), 1)
    ).!

  // an identifier wrapped in backticks. it can contain any char (e.g. `a b`, `a+b`).
  private def escapedIdentifier[$: P]: P[String] =
    P(
      "`" ~~ (!"`" ~~ AnyChar.!).repX(1) ~~ "`"
    ).map(_.mkString)

  // use only if the identifier is followed by a predefined character (e.g. `(` or `:`)
  private def identifierWithWhitespaces[$: P]: P[String] =
    P(
      identifier ~~ (" ".repX(1) ~~ identifier).repX(1)
    ).!

  private def name[$: P]: P[String] = P(
    identifier | escapedIdentifier
  )

  private def qualifiedName[$: P]: P[List[String]] =
    P(
      (valueProperty | name).rep(1, sep = ".")
    ).map(_.toList)

  // a string wrapped in double quotes. it can contain an escape sequences (e.g. \', \", \\, \n, \r, \t, \u269D, \U101EF).
  private def stringWithQuotes[$: P]: P[String] = P(
    "\"" ~~ (("\\" | !"\"") ~~ AnyChar).repX.! ~~ "\""
  )

  // --------------- utility parsers ---------------

  // shortcut function to define an optional parser
  private def optional[$: P](optionalParser: Exp => P[Exp]): Exp => P[Exp] = { base =>
    optionalParser(base).?.map(
      _.fold(base)(result => result)
    )
  }

  // --------------- expressions ---------------

  // use different levels to define the precedence of the operators  (i.e. how they can be chained)
  private def expression[$: P]: P[Exp] = expLvl1

  private def expLvl1[$: P]: P[Exp] = ifOp | forOp | quantifiedOp | disjunction

  private def expLvl2[$: P]: P[Exp] = conjunction

  private def expLvl3[$: P]: P[Exp] =
    expLvl4.flatMap(optional(comparison(_))) | simplePositiveUnaryTest

  private def expLvl4[$: P]: P[Exp] = mathOperator

  private def expLvl5[$: P]: P[Exp] = value

  // --------------- mathematical/arithmetic operators ---------------

  // use different levels to define the precedence of the operators
  private def mathOperator[$: P]: P[Exp] = mathOpLvl1

  private def mathOpLvl1[$: P]: P[Exp] = addSub

  private def mathOpLvl2[$: P]: P[Exp] = mulDiv

  private def mathOpLvl3[$: P]: P[Exp] = exponent

  private def mathOpLvl4[$: P]: P[Exp] = mathNegation

  // --------------- expression parsers ---------------

  private def ifOp[$: P]: P[Exp] =
    P(
      "if" ~ expression ~ "then" ~ expression ~ "else" ~ expression
    ).map { case (condition, thenExp, elseExp) =>
      If(condition, thenExp, elseExp)
    }

  private def forOp[$: P]: P[Exp] =
    P(
      "for" ~ listIterator.rep(1, sep = ",") ~ "return" ~ expression
    ).map { case (iterators, exp) =>
      For(iterators.toList, exp)
    }

  private def listIterator[$: P]: P[(String, Exp)] = P(
    name ~ "in" ~ (iterationContext | value)
  )

  private def iterationContext[$: P]: P[Exp] =
    P(
      expLvl4 ~ ".." ~ expLvl4
    ).map { case (start, end) =>
      IterationContext(start, end)
    }

  private def quantifiedOp[$: P]: P[Exp] =
    P(
      ("some" | "every").! ~ listIterator
        .rep(1, sep = ",") ~ "satisfies" ~ expression
    ).map {
      case ("some", iterators, condition)  =>
        SomeItem(iterators.toList, condition)
      case ("every", iterators, condition) =>
        EveryItem(iterators.toList, condition)
    }

  private def disjunction[$: P]: P[Exp] =
    P(
      expLvl2 ~ ("or" ~ expLvl2).rep
    ).map { case (base, ops) =>
      ops.foldLeft(base)(Disjunction.apply)
    }

  private def conjunction[$: P]: P[Exp] =
    P(
      expLvl3 ~ ("and" ~ expLvl3).rep
    ).map { case (base, ops) =>
      ops.foldLeft(base)(Conjunction.apply)
    }

  private def comparison[$: P](value: Exp): P[Exp] =
    binaryComparison(value) | between(value) | instanceOf(value) | in(value)

  private def binaryComparison[$: P](x: Exp): P[Exp] =
    P(
      StringIn("<=", ">=", "<", ">", "!=", "=").! ~ expLvl4
    ).map {
      case ("=", y)  => Equal(x, y)
      case ("!=", y) => Not(Equal(x, y))
      case ("<", y)  => LessThan(x, y)
      case ("<=", y) => LessOrEqual(x, y)
      case (">", y)  => GreaterThan(x, y)
      case (">=", y) => GreaterOrEqual(x, y)
    }

  private def between[$: P](x: Exp): P[Exp] =
    P(
      "between" ~ expLvl4 ~ "and" ~ expLvl4
    ).map { case (a, b) =>
      Conjunction(GreaterOrEqual(x, a), LessOrEqual(x, b))
    }

  private def instanceOf[$: P](value: Exp): P[Exp] =
    P(
      "instance" ~ "of" ~ typeName
    ).map(InstanceOf(value, _))

  private def typeName[$: P]: P[String] =
    P(
      specialTypeName |
        qualifiedName.map(_.mkString("."))
    )

  private def specialTypeName[$: P]: P[String] =
    P(
      "years and months duration" |
        "days and time duration" |
        "date and time" |
        "function"
    ).!

  private def in[$: P](value: Exp): P[Exp] =
    P(
      "in" ~ (("(" ~ positiveUnaryTests ~ ")") | positiveUnaryTest)
    ).map(In(value, _))

  // --------------- mathematical parsers ---------------

  private def addSub[$: P]: P[Exp] =
    P(
      mathOpLvl2 ~ (CharIn("+\\-").! ~ mathOpLvl2).rep
    ).map { case (value, ops) =>
      ops.foldLeft(value) {
        case (x, ("+", y)) => Addition(x, y)
        case (x, ("-", y)) => Subtraction(x, y)
      }
    }

  private def mulDiv[$: P]: P[Exp] =
    P(
      mathOpLvl3 ~ (CharIn("*/").! ~ mathOpLvl3).rep
    ).map { case (value, ops) =>
      ops.foldLeft(value) {
        case (x, ("*", y)) => Multiplication(x, y)
        case (x, ("/", y)) => Division(x, y)
      }
    }

  private def exponent[$: P]: P[Exp] =
    P(
      mathOpLvl4 ~ ("**" ~ mathOpLvl4).rep
    ).map { case (value, ops) =>
      ops.foldLeft(value)(Exponentiation.apply)
    }

  private def mathNegation[$: P]: P[Exp] =
    P(
      "-".!.? ~ expLvl5
    ).map {
      case (Some("-"), value) => ArithmeticNegation(value)
      case (None, value)      => value
    }

  // --------------- value/terminal parsers ---------------

  private def value[$: P]: P[Exp] =
    terminalValue.flatMap(optional(chainedValueOp(_)))

  private def terminalValue[$: P]: P[Exp] =
    temporal | functionInvocation | variableRef | literal | inputValue | functionDefinition | "(" ~ expression ~ ")"

  private def literal[$: P]: P[Exp] =
    nullLiteral | boolean | string | number | temporal | list | context

  private def nullLiteral[$: P]: P[Exp] =
    P(
      "null"
    ).map(_ => ConstNull)

  private def boolean[$: P]: P[Exp] =
    P(
      "true" | "false"
    ).!.map {
      case "true"  => ConstBool(true)
      case "false" => ConstBool(false)
    }

  private def string[$: P]: P[Exp] =
    P(
      stringWithQuotes
    ).map(translateEscapes).map(ConstString.apply)

  private def number[$: P]: P[Exp] =
    P(
      "-".? ~~ ((integral ~~ fractional.?) | fractional)
    ).!.map(number => ConstNumber(BigDecimal(number)))

  private def integral[$: P]: P[String] =
    P(
      CharIn("0-9") ~~ digits.?
    ).!

  private def fractional[$: P]: P[String] =
    P(
      "." ~~ digits
    ).!

  private def digits[$: P]: P[String] =
    P(
      CharsWhileIn("0-9")
    ).!

  private def temporal[$: P]: P[Exp] =
    P(
      (("duration" | "date and time" | "date" | "time").! ~ "(" ~ stringWithQuotes ~ ")") |
        ("@".! ~ stringWithQuotes)
    ).map {
      case ("duration", value)      => parseDuration(value)
      case ("date and time", value) => parseDateTime(value)
      case ("date", value)          => parseDate(value)
      case ("time", value)          => parseTime(value)
      case ("@", value)             => parseTemporalValue(value)
    }

  private def list[$: P]: P[Exp] =
    P(
      "[" ~ expression.rep(0, sep = ",") ~ "]"
    ).map(items => ConstList(items.toList))

  private def context[$: P]: P[Exp] =
    P(
      "{" ~ contextEntry.rep(0, sep = ",") ~ "}"
    ).map(entries => ConstContext(entries.toList))

  private def contextEntry[$: P]: P[(String, Exp)] = P(
    (contextKeyAnySymbol | identifierWithWhitespaces | name | stringWithQuotes) ~ ":" ~ expression
  )

  private def contextKeyAnySymbol[$: P]: P[String] =
    P(
      (!reservedSymbol ~ AnyChar).rep(1)
    ).!

  private def reservedSymbol[$: P]: P[String] =
    P(
      CharIn("\"", "{", "}", ":", ",", "[", "]", "`")
    ).!

  private def variableRef[$: P]: P[Exp] =
    P(
      qualifiedName
    ).map(Ref(_))

  private def inputValue[$: P]: P[Exp] =
    P(
      "?"
    ).map(_ => ConstInputValue)

  private def functionDefinition[$: P]: P[Exp] =
    P(
      "function" ~ "(" ~ parameter
        .rep(0, sep = ",") ~ ")" ~ (externalFunction | expression)
    ).map { case (parameters, body) =>
      FunctionDefinition(parameters.toList, body)
    }

  private def parameter[$: P]: P[String] = parameterName

  // parameter names from built-in functions can have whitespaces. the name is limited by `,` or `:`.
  private def parameterName[$: P]: P[String] = identifierWithWhitespaces | name

  private def externalFunction[$: P]: P[Exp] = P(
    "external" ~ externalJavaFunction
  )

  private def externalJavaFunction[$: P]: P[Exp] =
    P(
      "{" ~
        "java" ~ ":" ~ "{" ~
        "class" ~ ":" ~ stringWithQuotes ~ "," ~
        "method signature" ~ ":" ~ javaMethodSignature ~
        "}" ~ "}"
    ).map { case (className, (methodName, parameters)) =>
      JavaFunctionInvocation(className, methodName, parameters.toList)
    }

  private def javaMethodSignature[$: P]: P[(String, Seq[String])] = P(
    "\"" ~ name ~ "(" ~ javaMethodParameter_.rep(0, sep = ",") ~ ")" ~ "\""
  )

  private def javaMethodParameter_[$: P]: P[String] =
    P(
      qualifiedName
    ).map(_.mkString)

  private def functionInvocation[$: P]: P[Exp] =
    P(
      ((identifierWithWhitespaces | functionNameWithReservedWord)
        .map(List(_)) | qualifiedName) ~ "(" ~ functionParameters.? ~ ")"
    ).map {
      case (name :: Nil, None)             =>
        FunctionInvocation(name, PositionalFunctionParameters(List.empty))
      case (name :: Nil, Some(parameters)) =>
        FunctionInvocation(name, parameters)
      case (names, None)                   =>
        QualifiedFunctionInvocation(
          Ref(names.dropRight(1)),
          names.last,
          PositionalFunctionParameters(List.empty)
        )
      case (names, Some(parameters))       =>
        QualifiedFunctionInvocation(Ref(names.dropRight(1)), names.last, parameters)
    }

  // List all built-in function names that contains a reserved word. These names are not allowed as
  // regular function names.
  private def functionNameWithReservedWord[$: P]: P[String] =
    P(
      "and" | "or" | "date and time" | "years and months duration" | "get or else"
    ).!

  private def functionParameters[$: P]: P[FunctionParameters] =
    namedParameters | positionalParameters

  private def namedParameters[$: P]: P[NamedFunctionParameters] =
    P(
      (parameterName ~ ":" ~ (range | expression)).rep(1, sep = ",")
    ).map(params => NamedFunctionParameters(params.toMap))

  private def positionalParameters[$: P]: P[PositionalFunctionParameters] =
    P(
      (range | expression).rep(1, sep = ",")
    ).map(params => PositionalFunctionParameters(params.toList))

  // operators of values that can be chained multiple times (e.g. `a.b.c`, `a[1][2]`, `a.b[1].c`)
  private def chainedValueOp[$: P](value: Exp): P[Exp] =
    (path(value) | filter(value)).flatMap(optional(chainedValueOp(_)))

  private def path[$: P](value: Exp): P[Exp] =
    P(
      ("." ~ (valueProperty | name)).rep(1)
    ).map(ops => ops.foldLeft(value)(PathExpression.apply))

  // list all properties that doesn't match to the regular name (i.e. with whitespaces)
  // - generic parser with whitespace doesn't work because there is no fixed follow-up character
  private def valueProperty[$: P]: P[String] =
    P(
      "time offset"
    ).!

  private def filter[$: P](base: Exp): P[Exp] =
    P(
      ("[" ~ expression ~ "]").rep(1)
    ).map(ops => ops.foldLeft(base)(Filter.apply))

  // --------------- unary-tests expressions ---------------

  private def unaryTests[$: P]: P[Exp] =
    negation | positiveUnaryTests | anyInput

  private def negation[$: P]: P[Exp] =
    P(
      "not" ~ "(" ~ positiveUnaryTests ~ ")"
    ).map(Not.apply)

  private def positiveUnaryTests[$: P]: P[Exp] =
    P(
      positiveUnaryTest.rep(1, sep = ",")
    ).map {
      case test :: Nil => test
      case tests       => AtLeastOne(tests.toList)
    }

  // Expressions are a subset of positive-unary-test. However, we need to give precedence to the
  // following cases to avoid ambiguous behavior:
  // - comparison with a boolean (e.g. `true`, `false`)
  // - comparison with less or greater than (e.g. `< 3`, `>= 5`)
  // - comparison with an interval (e.g. `[2..5]`)
  private def positiveUnaryTest[$: P]: P[Exp] =
    (boolean.map(InputEqualTo.apply) ~ End) |
      simplePositiveUnaryTest |
      expression.map(UnaryTestExpression.apply)

  private def anyInput[$: P]: P[Exp] =
    P(
      "-"
    ).map(_ => ConstBool(true))

  private def simplePositiveUnaryTest[$: P]: P[Exp] = unaryComparison | interval

  private def unaryComparison[$: P]: P[Exp] =
    P(
      StringIn("<=", ">=", "<", ">").! ~ endpoint
    ).map {
      case ("<", x)  => InputLessThan(x)
      case ("<=", x) => InputLessOrEqual(x)
      case (">", x)  => InputGreaterThan(x)
      case (">=", x) => InputGreaterOrEqual(x)
    }

  // allow more expressions compared to the spec to align unary-tests with other expression
  private def endpoint[$: P]: P[Exp] = expLvl4

  private def interval[$: P]: P[Exp] = range.map(InputInRange.apply)

  private def range[$: P]: P[ConstRange] =
    P(
      rangeStart ~ ".." ~ rangeEnd
    ).map { case (start, end) =>
      ConstRange(start, end)
    }

  private def rangeStart[$: P]: P[ConstRangeBoundary] =
    P(
      CharIn("(", "]", "[").! ~ expLvl4
    ).map {
      case ("(", x) => OpenConstRangeBoundary(x)
      case ("]", x) => OpenConstRangeBoundary(x)
      case ("[", x) => ClosedConstRangeBoundary(x)
    }

  private def rangeEnd[$: P]: P[ConstRangeBoundary] =
    P(
      expLvl4 ~ CharIn(")", "[", "]").!
    ).map {
      case (y, ")") => OpenConstRangeBoundary(y)
      case (y, "[") => OpenConstRangeBoundary(y)
      case (y, "]") => ClosedConstRangeBoundary(y)
    }

  // --------------- temporal parsers ---------------

  private def parseDate(d: String): Exp = {
    Try(ConstDate(d)).filter(_ => isValidDate(d)).getOrElse(ConstNull)
  }

  private def parseTime(t: String): Exp = {
    Try {
      if (isOffsetTime(t)) {
        ConstTime(t)
      } else {
        ConstLocalTime(t)
      }
    }.getOrElse(ConstNull)
  }

  private def parseDateTime(dt: String): Exp = {
    Try {
      if (isValidDate(dt)) {
        ConstLocalDateTime((dt: Date).atTime(0, 0))
      } else if (isOffsetDateTime(dt)) {
        ConstDateTime(dt)
      } else {
        ConstLocalDateTime(dt)
      }
    }.getOrElse(ConstNull)
  }

  private def parseDuration(d: String): Exp = {
    Try {
      if (isYearMonthDuration(d)) {
        ConstYearMonthDuration(d)
      } else {
        ConstDayTimeDuration(d)
      }
    }.getOrElse(ConstNull)
  }

  private def parseTemporalValue(value: String): Exp = {
    Try {
      if (isValidDate(value)) {
        ConstDate(value)
      } else if (isOffsetTime(value)) {
        ConstTime(value)
      } else if (isOffsetDateTime(value)) {
        ConstDateTime(value)
      } else if (isLocalDateTime(value)) {
        ConstLocalDateTime(value)
      } else if (isYearMonthDuration(value)) {
        ConstYearMonthDuration(value)
      } else if (isDayTimeDuration(value)) {
        ConstDayTimeDuration(value)
      } else {
        ConstLocalTime(value)
      }
    }.getOrElse(ConstNull)
  }

  private def translateEscapes(input: String): String = {
    // Replace escape sequences without using lookbehind (not supported in Scala Native).
    // Strategy: first protect escaped backslashes, then replace escapes, then restore.
    val placeholder = "\u0000BACKSLASH\u0000" // unlikely to appear in input
    input
      .replace("\\\\", placeholder) // protect escaped backslashes
      .replace("\\n", "\n")         // new line
      .replace("\\r", "\r")         // carriage return
      .replace("\\t", "\t")         // tab
      .replace("\\b", "\b")         // backspace
      .replace("\\f", "\f")         // form feed
      .replace("\\'", "'")          // single quote
      .replace("\\\"", "\"")        // double quote
      .replace(placeholder, "\\")   // restore backslashes
  }
}
