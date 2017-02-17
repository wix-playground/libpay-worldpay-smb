package com.wix.pay.worldpay.smb

import com.wix.pay.worldpay.smb.testkit.WorldpaySmbDriver
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import spray.http.StatusCodes

class WorldpaySmbGatewayIT extends SpecWithJUnit with WorldpayTestSupport {
  val probePort = 10001
  val driver = new WorldpaySmbDriver(probePort)

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
      givenWorldpayAuthorizationRequest isRejectedWith(someOrderCode, "Some error message")
      authorize() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentRejectedException for 'Bad Request 400' response" in new Ctx {
      givenWorldpayAuthorizationRequest isAnErrorWith(StatusCodes.BadRequest, "Some error message")
      authorize() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentErrorException for erroneous response" in new Ctx {
      givenWorldpayAuthorizationRequest isAnErrorWith(StatusCodes.Unauthorized, "Something bad happened")
      authorize() must failWithMessage("Something bad happened")
    }
  }

  "capture request" should {
    "successfully yield an orderCode upon a valid request" in new Ctx {
      givenWorldpayCaptureRequest returns someOrderCode
      capture() must beSuccessfulTry.withValue(someOrderCode)
    }

    "fail with PaymentRejectedException for 'Bad Request 400' response" in new Ctx {
      givenWorldpayCaptureRequest isAnErrorWith(StatusCodes.BadRequest, "Some error message")
      capture() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentErrorException for erroneous response" in new Ctx {
      givenWorldpayCaptureRequest isAnErrorWith(StatusCodes.Unauthorized, "Something bad happened")
      capture() must failWithMessage("Something bad happened")
    }
  }

  "sale request" should {
    "successfully yield an authorization key upon a valid request" in new Ctx {
      givenWorldpaySaleRequest returns someOrderCode
      sale() must beSuccessfulTry.withValue(someOrderCode)
    }

    "fail with PaymentRejectedException for rejected transactions" in new Ctx {
      givenWorldpaySaleRequest isRejectedWith(someOrderCode, "Some error message")
      sale() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentRejectedException for 'Bad Request 400' response" in new Ctx {
      givenWorldpaySaleRequest isAnErrorWith(StatusCodes.BadRequest, "Some error message")
      sale() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentErrorException for erroneous response" in new Ctx {
      givenWorldpaySaleRequest isAnErrorWith(StatusCodes.Unauthorized, "Something bad happened")
      sale() must failWithMessage("Something bad happened")
    }
  }

  "voidAuthorization request" should {
    "successfully yield an authorization key upon a valid request" in new Ctx {
      givenWorldpayVoidAuthorizationRequest returns someOrderCode
      voidAuthorization() must beSuccessfulTry.withValue(someOrderCode)
    }

    "fail with PaymentRejectedException for 'Bad Request 400' response" in new Ctx {
      givenWorldpayVoidAuthorizationRequest isAnErrorWith(StatusCodes.BadRequest, "Some error message")
      voidAuthorization() must beRejectedWithMessage("Some error message")
    }

    "fail with PaymentErrorException for erroneous response" in new Ctx {
      givenWorldpayVoidAuthorizationRequest isAnErrorWith(StatusCodes.Unauthorized, "Something bad happened")
      voidAuthorization() must failWithMessage("Something bad happened")
    }
  }

  step {
    driver.stop()
  }

  trait Ctx extends Scope {
    val gateway = new WorldpaySmbGateway(s"http://localhost:$probePort")

    driver.reset()

    def givenWorldpayAuthorizationRequest = driver.anAuthorizationRequest(serviceKey, settlementCurrency, creditCard, currencyAmount, Some(deal))
    def authorize() = gateway.authorize(someMerchantStr, creditCard, payment, None, Some(deal))

    def givenWorldpayCaptureRequest = driver.aCaptureRequest(serviceKey, someOrderCode, creditCard, currencyAmount, Some(deal))
    def capture() = gateway.capture(someMerchantStr, someAuthorization, currencyAmount.amount)

    def givenWorldpaySaleRequest = driver.aSaleRequest(serviceKey, settlementCurrency, creditCard, currencyAmount, Some(deal))
    def sale() = gateway.sale(someMerchantStr, creditCard, payment, None, Some(deal))

    def givenWorldpayVoidAuthorizationRequest = driver.aVoidAuthorizationRequest(serviceKey, someOrderCode)
    def voidAuthorization() = gateway.voidAuthorization(someMerchantStr, someAuthorization)
  }
}
