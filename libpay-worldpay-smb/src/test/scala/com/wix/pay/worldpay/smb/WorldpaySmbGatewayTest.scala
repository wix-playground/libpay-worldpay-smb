package com.wix.pay.worldpay.smb

import com.wix.pay.creditcard._
import com.wix.pay.model.Payment
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class WorldpaySmbGatewayTest extends SpecWithJUnit with WorldpayTestSupport {
  "authorize request" should {
    "fail for invalid merchant format" in new Ctx {
      authorize(merchantKey = "invalid") must beParseError
    }

    "fail if credit card csc is not provided" in new Ctx {
      authorize(creditCard = someCreditCard.withoutCsc) must failWithMessage("Credit Card CSC is mandatory for Worldpay!")
    }

    "fail if credit card holder name is not provided" in new Ctx {
      authorize(creditCard = someCreditCard.withoutHolderName) must failWithMessage("Credit Card Holder Name is mandatory for Worldpay!")
    }

    "fail if payment has more than 1 installment" in new Ctx {
      authorize(payment = somePayment.withInstallments(2)) must failWithMessage("Worldpay does not support installments!")
    }
  }

  "capture request" should {
    "fail on invalid merchant format" in new Ctx {
      capture(merchantKey = "invalid") must beParseError
    }

    "fail on invalid authorization format" in new Ctx {
      capture(authorization = "invalid") must beParseError
    }
  }

  "sale request" should {
    "fail for invalid merchant format" in new Ctx {
      sale(merchantKey = "invalid") must beParseError
    }

    "fail if credit card csc is not provided" in new Ctx {
      sale(creditCard = someCreditCard.withoutCsc) must failWithMessage("Credit Card CSC is mandatory for Worldpay!")
    }

    "fail if credit card holder name is not provided" in new Ctx {
      sale(creditCard = someCreditCard.withoutHolderName) must failWithMessage("Credit Card Holder Name is mandatory for Worldpay!")
    }

    "fail if payment has more than 1 installment" in new Ctx {
      sale(payment = somePayment.withInstallments(2)) must failWithMessage("Worldpay does not support installments!")
    }
  }

  "void request" should {
    "fail on invalid merchant format" in new Ctx {
      capture(merchantKey = "invalid") must beParseError
    }

    "fail on invalid authorization format" in new Ctx {
      void(authorization = "invalid") must beParseError
    }
  }

  trait Ctx extends Scope {
    val worldpayGateway = new WorldpaySmbGateway("")

    def authorize(merchantKey: String = someMerchantStr, creditCard: CreditCard = someCreditCard, payment: Payment = somePayment) =
      worldpayGateway.authorize(merchantKey, creditCard, payment, customer = None, deal = None)

    def capture(merchantKey: String = someMerchantStr, authorization: String = someAuthorization) =
      worldpayGateway.capture(merchantKey, authorization, someCurrencyAmount.amount)

    def sale(merchantKey: String = someMerchantStr, creditCard: CreditCard = someCreditCard, payment: Payment = somePayment) =
      worldpayGateway.sale(merchantKey, creditCard, payment, customer = None, deal = None)

    def void(merchantKey: String = someMerchantStr, authorization: String = someAuthorization) =
      worldpayGateway.voidAuthorization(merchantKey, authorization)
  }
}
