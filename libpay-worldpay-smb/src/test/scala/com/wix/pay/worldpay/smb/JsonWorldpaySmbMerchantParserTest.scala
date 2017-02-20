package com.wix.pay.worldpay.smb

import com.wix.pay.worldpay.smb.parsers.JsonWorldpaySmbMerchantParser
import org.specs2.mutable.SpecWithJUnit

class JsonWorldpaySmbMerchantParserTest extends SpecWithJUnit {
  val parser = JsonWorldpaySmbMerchantParser
  val serviceKey = "someServiceKey"
  val settlementCurrency = "someSettlementCurrency"

  "stringify and then parse" should {
    "return an order similar to the original one" in {
      val merchant = WorldpaySmbMerchant(serviceKey, settlementCurrency)
      val merchantKey = parser.stringify(merchant)

      parser.parse(merchantKey) must be_==(merchant)
    }
  }
}
