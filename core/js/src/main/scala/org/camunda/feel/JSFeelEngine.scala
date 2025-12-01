package org.camunda.feel

import org.camunda.feel.api.{
  EvaluationFailure,
  EvaluationResult,
  FailedEvaluationResult,
  FeelEngineApi,
  SuccessfulEvaluationResult
}

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import org.camunda.feel.impl.interpreter.ObjectContext
import org.camunda.feel.valuemapper.JSValueMapper

@JSExportAll
@JSExportTopLevel("JSFeelEngine")
class JSFeelEngine() {

  private val valueMapper = new JSValueMapper()
  private val engine      = new FeelEngine(valueMapper = valueMapper)

  private val api = new FeelEngineApi(engine)

  def evaluate(expression: String, context: js.Dynamic): js.Dynamic = {
    val ctx = ObjectContext(context)
    api.evaluateExpression(expression, ctx) match {
      case SuccessfulEvaluationResult(result, suppressedFailures) =>
        js.Dynamic.literal(
          result = result.asInstanceOf[js.Any],
          isSuccess = true,
          warnings = suppressedFailures.map(convertWarning).toJSArray
        )
      case FailedEvaluationResult(failure, suppressedFailures)    =>
        js.Dynamic.literal(
          failure = failure.message,
          isSuccess = false,
          warnings = suppressedFailures.map(convertWarning).toJSArray
        )
    }
  }

  def evaluateUnaryTests(expression: String, context: js.Dynamic): EvaluationResult = {
    val ctx = ObjectContext(context)
    api.evaluateUnaryTests(expression, ctx)
  }

  def convertWarning(failure: EvaluationFailure): js.Dynamic = {
    js.Dynamic.literal(message = failure.failureMessage, `type` = failure.failureType.toString)
  }

}
