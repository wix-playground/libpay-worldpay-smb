package com.wix.pay.worldpay.smb


import scala.util.Try
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import akka.http.scaladsl.model.StatusCodes
import com.wix.pay.worldpay.smb.testkit.WorldpaySmbDriver


class WorldpaySmbGatewayIT extends SpecWithJUnit with WorldpayTestSupport {
  val probePort = 10001
  val driver = new WorldpaySmbDriver(probePort)
  val gateway = new WorldpaySmbGateway(s"http://localhost:$probePort")

  def givenWorldpayAuthorizationRequest: driver.AuthorizationRequest = driver.anAuthorizationRequest(
    serviceKey, settlementCurrency, someCreditCard, someCurrencyAmount, Some(someDeal))
  def authorize(): Try[String] = gateway.authorize(someMerchantStr, someCreditCard, somePayment, None, Some(someDeal))

  def givenWorldpayCaptureRequest: driver.CaptureRequest = driver.aCaptureRequest(
    serviceKey, someOrderCode, someCreditCard, someCurrencyAmount, Some(someDeal))
  def capture(): Try[String] = gateway.capture(someMerchantStr, someAuthorization, someCurrencyAmount.amount)

  def givenWorldpaySaleRequest: driver.AuthorizationRequest = driver.aSaleRequest(
    serviceKey, settlementCurrency, someCreditCard, someCurrencyAmount, Some(someDeal))
  def sale(): Try[String] = gateway.sale(someMerchantStr, someCreditCard, somePayment, None, Some(someDeal))

  def givenWorldpayVoidAuthorizationRequest: driver.VoidAuthorizationRequest = driver.aVoidAuthorizationRequest(
    serviceKey, someOrderCode)
  def voidAuthorization(): Try[String] = gateway.voidAuthorization(someMerchantStr, someAuthorization)


  step {
    driver.start()
  }


  sequential


  "authorize request" should {
    "successfully yield an authorization key upon a valid request" in new Ctx {
      givenWorldpayAuthorizationRequest returns someOrderCode
      authorize() must beSuccessfulTry.withValue(someAuthorization)
    }

    "fail with PaymentRejectedException for rejected transactions" in new Ctx {
      givenWorldpayAuthorizationRequest getsRejectedWith(someOrderCode, "Some error message")
      authorize() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentRejectedException for 'Bad Request 400' response" in new Ctx {
      givenWorldpayAuthorizationRequest getsAnErrorWith(StatusCodes.BadRequest, "Some error message")
      authorize() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentErrorException for erroneous response" in new Ctx {
      givenWorldpayAuthorizationRequest getsAnErrorWith(StatusCodes.Unauthorized, "Something bad happened")
      authorize() must failWithMessage("Something bad happened")
    }
  }


  "capture request" should {
    "successfully yield an orderCode upon a valid request" in new Ctx {
      givenWorldpayCaptureRequest returns someOrderCode
      capture() must beSuccessfulTry.withValue(someOrderCode)
    }

    "fail with PaymentRejectedException for 'Bad Request 400' response" in new Ctx {
      givenWorldpayCaptureRequest getsAnErrorWith(StatusCodes.BadRequest, "Some error message")
      capture() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentErrorException for erroneous response" in new Ctx {
      givenWorldpayCaptureRequest getsAnErrorWith(StatusCodes.Unauthorized, "Something bad happened")
      capture() must failWithMessage("Something bad happened")
    }
  }


  "sale request" should {
    "successfully yield an authorization key upon a valid request" in new Ctx {
      givenWorldpaySaleRequest returns someOrderCode
      sale() must beSuccessfulTry.withValue(someOrderCode)
    }

    "fail with PaymentRejectedException for rejected transactions" in new Ctx {
      givenWorldpaySaleRequest getsRejectedWith(someOrderCode, "Some error message")
      sale() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentRejectedException for 'Bad Request 400' response" in new Ctx {
      givenWorldpaySaleRequest getsAnErrorWith(StatusCodes.BadRequest, "Some error message")
      sale() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentErrorException for erroneous response" in new Ctx {
      givenWorldpaySaleRequest getsAnErrorWith(StatusCodes.Unauthorized, "Something bad happened")
      sale() must failWithMessage("Something bad happened")
    }
  }


  "voidAuthorization request" should {
    "successfully yield an authorization key upon a valid request" in new Ctx {
      givenWorldpayVoidAuthorizationRequest returns someOrderCode
      voidAuthorization() must beSuccessfulTry.withValue(someOrderCode)
    }

    "fail with PaymentRejectedException for 'Bad Request 400' response" in new Ctx {
      givenWorldpayVoidAuthorizationRequest getsAnErrorWith(StatusCodes.BadRequest, "Some error message")
      voidAuthorization() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentErrorException for erroneous response" in new Ctx {
      givenWorldpayVoidAuthorizationRequest getsAnErrorWith(StatusCodes.Unauthorized, "Something bad happened")
      voidAuthorization() must failWithMessage("Something bad happened")
    }
  }


  step {
    driver.stop()
  }


  trait Ctx extends Scope {
    driver.reset()
  }
}
