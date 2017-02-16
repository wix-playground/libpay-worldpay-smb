package com.wix.pay.worldpay.smb

import java.util.Locale

import com.wix.pay.creditcard._
import com.wix.pay.model.{CurrencyAmount, Deal, Payment, ShippingAddress}
import com.wix.pay.worldpay.smb.parsers.{JsonWorldpaySmbAuthorizationParser, JsonWorldpaySmbMerchantParser}
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.json4s.ParserUtil.ParseException
import org.specs2.matcher.Matcher
import org.specs2.matcher.MustThrownMatchers._

import scala.util.Try

trait WorldpayTestSupport {
  val serviceKey = "someServiceKey"
  val someMerchant = JsonWorldpaySmbMerchantParser.stringify(WorldpaySmbMerchant(serviceKey))

  val someOrderCode = "$$$"
  val someAuthorization = JsonWorldpaySmbAuthorizationParser.stringify(WorldpaySmbAuthorization(someOrderCode))
  
  val creditCard = CreditCard("4580458045804580", YearMonth(2020, 12), Some(CreditCardOptionalFields(
    csc = Some("123"),
    publicFields = Some(PublicCreditCardOptionalFields(
      holderId = None,
      holderName = Some("Some Name"),
      billingAddressDetailed = Some(AddressDetailed(
        street = Some("billingStreet"),
        city = Some("billingCity"),
        state = Some("billingState"),
        postalCode = Some("billingPostalCode"),
        countryCode = Some(Locale.GERMANY)
      ))
    ))
  )))

  val deal = Deal(
    id = "123",
    title = Some("title"),
    description = Some("desc"),
    invoiceId = Some("invoiceId"),
    shippingAddress = Some(ShippingAddress(
      firstName = Some("firstName"),
      lastName = Some("lastName"),
      address = Some(AddressDetailed(
        street = Some("shippingStreet"),
        city = Some("shippingCity"),
        postalCode = Some("shippingPostalCode"),
        state = Some("shippingState"),
        countryCode = Some(Locale.CHINA)
      ))
    )))

  val currencyAmount = CurrencyAmount("USD", 5.67)
  val payment = Payment(currencyAmount, installments = 1)
  
  def beRejectedWithMessage(message: String): Matcher[Try[String]] = beFailedTry.like { case e: PaymentRejectedException => e.message must contain(message) }
  def failWithMessage(message: String): Matcher[Try[String]] = beFailedTry.like { case e: PaymentErrorException => e.message must contain(message) }
  def beParseError: Matcher[Try[String]] = beFailedTry.like { case e: PaymentErrorException => e.cause must beAnInstanceOf[ParseException] }

  implicit class DealTestExtensions(o: Deal) {
    def withDescription(description: String) = o.copy(description = Some(description))
    def withoutDescription = o.copy(description = None)
    
    def withTitle(title: String) = o.copy(title = Some(title))
    def withoutTitle = o.copy(title = None)
  }
}
