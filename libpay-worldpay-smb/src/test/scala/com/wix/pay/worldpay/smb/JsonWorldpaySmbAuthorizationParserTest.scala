package com.wix.pay.worldpay.smb

import com.wix.pay.worldpay.smb.parsers.JsonWorldpaySmbAuthorizationParser
import org.specs2.mutable.SpecWithJUnit

class JsonWorldpaySmbAuthorizationParserTest extends SpecWithJUnit {
  val parser = JsonWorldpaySmbAuthorizationParser
  val orderCode = "123"

  "stringify and then parse" should {
    "return an order similar to the original one" in {
      val authorization = WorldpaySmbAuthorization(orderCode)
      val authorizationKey = parser.stringify(authorization)

      parser.parse(authorizationKey) must be_==(authorization)
    }
  }
}
