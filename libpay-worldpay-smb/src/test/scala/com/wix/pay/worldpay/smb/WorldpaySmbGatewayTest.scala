package com.wix.pay.worldpay.smb

import com.wix.pay.PaymentErrorException
import com.wix.pay.creditcard._
import com.wix.pay.model.CurrencyAmount
import com.wix.pay.worldpay.smb.parsers.{JsonWorldpaySmbAuthorizationParser, JsonWorldpaySmbMerchantParser}
import org.json4s.ParserUtil.ParseException
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class WorldpaySmbGatewayTest extends SpecWithJUnit {
  "authorize request" should {
    "fail for invalid merchant format" in new Ctx {
      authorize(merchantKey = "invalid") must
        beFailedTry.like { case e: PaymentErrorException => e.cause must beAnInstanceOf[ParseException] }
    }

    "fail if credit card csc is not provided" in new Ctx {
      val creditCardWithoutCsc = creditCard.copy(additionalFields = Some(CreditCardOptionalFields.withFields(
        holderName = Some("Some Name")
      )))
      authorize(creditCard = creditCardWithoutCsc) must
        beFailedTry.like { case e: PaymentErrorException => e.message must contain("Credit Card CSC is mandatory for Worldpay!") }
    }

    "fail if credit card holder name is not provided" in new Ctx {
      val creditCardWithoutHolderName = creditCard.copy(additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123")
      )))
      authorize(creditCard = creditCardWithoutHolderName) must
        beFailedTry.like { case e: PaymentErrorException => e.message must contain("Credit Card Holder Name is mandatory for Worldpay!") }
    }
  }

  "capture request" should {
    "fail on invalid merchant format" in new Ctx {
      capture(merchantKey = "invalid") must
        beFailedTry.like { case e: PaymentErrorException => e.cause must beAnInstanceOf[ParseException] }
    }

    "fail on invalid authorization format" in new Ctx {
      capture(authorization = "invalid") must
        beFailedTry.like { case e: PaymentErrorException => e.cause must beAnInstanceOf[ParseException] }
    }
  }

  "sale request" should {
    "fail for invalid merchant format" in new Ctx {
      sale(merchantKey = "invalid") must
        beFailedTry.like { case e: PaymentErrorException => e.cause must beAnInstanceOf[ParseException] }
    }

    "fail if credit card csc is not provided" in new Ctx {
      val creditCardWithoutCsc = creditCard.copy(additionalFields = Some(CreditCardOptionalFields.withFields(
        holderName = Some("Some Name")
      )))
      sale(creditCard = creditCardWithoutCsc) must
        beFailedTry.like { case e: PaymentErrorException => e.message must contain("Credit Card CSC is mandatory for Worldpay!") }
    }

    "fail if credit card holder name is not provided" in new Ctx {
      val creditCardWithoutHolderName = creditCard.copy(additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123")
      )))
      sale(creditCard = creditCardWithoutHolderName) must
        beFailedTry.like { case e: PaymentErrorException => e.message must contain("Credit Card Holder Name is mandatory for Worldpay!") }
    }
  }

  "void request" should {
    "fail on invalid merchant format" in new Ctx {
      capture(merchantKey = "invalid") must
        beFailedTry.like { case e: PaymentErrorException => e.cause must beAnInstanceOf[ParseException] }
    }

    "fail on invalid authorization format" in new Ctx {
      void(authorization = "invalid") must
        beFailedTry.like { case e: PaymentErrorException => e.cause must beAnInstanceOf[ParseException] }
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

    def authorize(merchantKey: String = someMerchant, creditCard: CreditCard = creditCard) =
      worldpayGateway.authorize(merchantKey, creditCard, currencyAmount, customer = None, deal = None)

    def capture(merchantKey: String = someMerchant, authorization: String = someAuthorization) =
      worldpayGateway.capture(merchantKey, authorization, currencyAmount.amount)

    def sale(merchantKey: String = someMerchant, creditCard: CreditCard = creditCard) =
      worldpayGateway.sale(merchantKey, creditCard, currencyAmount, customer = None, deal = None)

    def void(merchantKey: String = someMerchant, authorization: String = someAuthorization) =
      worldpayGateway.voidAuthorization(merchantKey, authorization)
  }
}
