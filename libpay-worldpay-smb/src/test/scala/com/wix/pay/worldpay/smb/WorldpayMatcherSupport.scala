package com.wix.pay.worldpay.smb

import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.json4s.ParserUtil.ParseException
import org.specs2.matcher.Matcher
import org.specs2.matcher.MustThrownMatchers._

import scala.util.Try

trait WorldpayMatcherSupport {
  def beRejectedWithMessage(message: String): Matcher[Try[String]] = beFailedTry.like { case e: PaymentRejectedException => e.message mustEqual message }
  def failWithMessage(message: String): Matcher[Try[String]] = beFailedTry.like { case e: PaymentErrorException => e.message must contain(message) }
  def beParseError: Matcher[Try[String]] = beFailedTry.like { case e: PaymentErrorException => e.cause must beAnInstanceOf[ParseException] }
}
