package com.wix.pay.worldpay.smb

import com.wix.pay.creditcard._
import com.wix.pay.model.{CurrencyAmount, Payment}
import com.wix.pay.worldpay.smb.parsers.{JsonWorldpaySmbAuthorizationParser, JsonWorldpaySmbMerchantParser}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class WorldpaySmbGatewayTest extends SpecWithJUnit with WorldpayMatcherSupport {
  "authorize request" should {
    "fail for invalid merchant format" in new Ctx {
      authorize(merchantKey = "invalid") must beParseError
    }

    "fail if credit card csc is not provided" in new Ctx {
      val creditCardWithoutCsc = creditCard.copy(additionalFields = Some(CreditCardOptionalFields.withFields(
        holderName = Some("Some Name")
      )))
      authorize(creditCard = creditCardWithoutCsc) must failWithMessage("Credit Card CSC is mandatory for Worldpay!")
    }

    "fail if credit card holder name is not provided" in new Ctx {
      val creditCardWithoutHolderName = creditCard.copy(additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123")
      )))
      authorize(creditCard = creditCardWithoutHolderName) must failWithMessage("Credit Card Holder Name is mandatory for Worldpay!")
    }

    "fail if payment has more than 1 installment" in new Ctx {
      val paymentWithInvalidInstallments = payment.copy(installments = 2)
      authorize(payment = paymentWithInvalidInstallments) must failWithMessage("Worldpay does not support installments!")
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
      val creditCardWithoutCsc = creditCard.copy(additionalFields = Some(CreditCardOptionalFields.withFields(
        holderName = Some("Some Name")
      )))
      sale(creditCard = creditCardWithoutCsc) must failWithMessage("Credit Card CSC is mandatory for Worldpay!")
    }

    "fail if credit card holder name is not provided" in new Ctx {
      val creditCardWithoutHolderName = creditCard.copy(additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123")
      )))
      sale(creditCard = creditCardWithoutHolderName) must failWithMessage("Credit Card Holder Name is mandatory for Worldpay!")
    }

    "fail if payment has more than 1 installment" in new Ctx {
      val paymentWithInvalidInstallments = payment.copy(installments = 2)
      sale(payment = paymentWithInvalidInstallments) must failWithMessage("Worldpay does not support installments!")
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

    val serviceKey = "someServiceKey"
    val someMerchant = JsonWorldpaySmbMerchantParser.stringify(WorldpaySmbMerchant(serviceKey))

    val someOrderCode = "$$$"
    val someAuthorization = JsonWorldpaySmbAuthorizationParser.stringify(WorldpaySmbAuthorization(someOrderCode))

    val creditCard = CreditCard("4580458045804580", YearMonth(2020, 12), Some(CreditCardOptionalFields.withFields(
      csc = Some("123"), holderName = Some("name")
    )))

    val currencyAmount = CurrencyAmount("USD", 5.67)
    val payment = Payment(currencyAmount, installments = 1)

    def authorize(merchantKey: String = someMerchant, creditCard: CreditCard = creditCard, payment: Payment = payment) =
      worldpayGateway.authorize(merchantKey, creditCard, payment, customer = None, deal = None)

    def capture(merchantKey: String = someMerchant, authorization: String = someAuthorization) =
      worldpayGateway.capture(merchantKey, authorization, currencyAmount.amount)

    def sale(merchantKey: String = someMerchant, creditCard: CreditCard = creditCard, payment: Payment = payment) =
      worldpayGateway.sale(merchantKey, creditCard, payment, customer = None, deal = None)

    def void(merchantKey: String = someMerchant, authorization: String = someAuthorization) =
      worldpayGateway.voidAuthorization(merchantKey, authorization)
  }
}
