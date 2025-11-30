package org.camunda.feel

import org.camunda.feel.api.FeelEngineApi

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSExportAll}
import org.camunda.feel.impl.interpreter.ObjectContext
import org.camunda.feel.api.SuccessfulEvaluationResult
import org.camunda.feel.api.FailedEvaluationResult
import org.camunda.feel.api.EvaluationResult
import org.camunda.feel.valuemapper.ValueMapper
import org.camunda.feel.impl.DefaultValueMapper
import org.camunda.feel.syntaxtree.Val
import org.camunda.feel.syntaxtree.ValNumber
import org.camunda.feel.syntaxtree.ValBoolean
import org.camunda.feel.syntaxtree.ValString
import org.camunda.feel.syntaxtree.ValDate
import org.camunda.feel.syntaxtree.ValLocalTime
import org.camunda.feel.syntaxtree.ValTime
import org.camunda.feel.syntaxtree.ValLocalDateTime
import org.camunda.feel.syntaxtree.ValDateTime
import org.camunda.feel.syntaxtree.ValYearMonthDuration
import org.camunda.feel.syntaxtree.ValDayTimeDuration
import org.camunda.feel.syntaxtree.ValError
import org.camunda.feel.syntaxtree.ValFatalError
import org.camunda.feel.syntaxtree.ValNull
import org.camunda.feel.syntaxtree.ValFunction
import org.camunda.feel.syntaxtree.ValContext
import org.camunda.feel.syntaxtree.ValList
import org.camunda.feel.syntaxtree.ValRange

@JSExportAll
@JSExportTopLevel("JSFeelEngine")
class JSFeelEngine() {
  private val valueMapper = {
    val default = DefaultValueMapper.instance

    new ValueMapper {

      override def toVal(x: Any): Val = default.toVal(x)

      override def unpackVal(value: Val): Any = {
        val jsAny: js.Any = value match {
          case ValNumber(value)                        => value.toDouble
          case ValBoolean(value)                       => value
          case ValString(value)                        => value
          case ValDate(value)                          => value.toString
          case ValLocalTime(value)                     => value.toString()
          case ValTime(value)                          => value.toString()
          case ValLocalDateTime(value)                 => value.toString()
          case ValDateTime(value)                      => value.toString()
          case ValYearMonthDuration(value)             => value.toString()
          case ValDayTimeDuration(value)               => value.toString()
          case ValError(error)                         => ???
          case ValFatalError(error)                    => ???
          case ValNull                                 => null
          case ValFunction(params, invoke, hasVarArgs) => null
          case ValContext(context)                     => ???
          case ValList(itemsAsSeq)                     => itemsAsSeq.map(unpackVal)
          case ValRange(start, end)                    => null
        }
        jsAny
      }

    }
  }

  private val engine = new FeelEngine(valueMapper = valueMapper)

  private val api = new FeelEngineApi(engine)

  def evaluate(expression: String, context: js.Dynamic): EvaluationResult = {
    val ctx    = ObjectContext(context)
    var result = api.evaluateExpression(expression, ctx)
    result match {
      case SuccessfulEvaluationResult(result, suppressedFailures) =>
        println(s"Result is $result : ${result.getClass.getSimpleName}")

      case FailedEvaluationResult(failure, suppressedFailures) =>

    }
    result
  }
}
