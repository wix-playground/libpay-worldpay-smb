package com.wix.pay.worldpay.smb.parsers

import com.wix.pay.worldpay.smb.WorldpaySmbMerchant
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

object JsonWorldpaySmbMerchantParser extends WorldpaySmbMerchantParser {
  private implicit val formats = DefaultFormats

  override def parse(merchantKey: String): WorldpaySmbMerchant = {
    Serialization.read[WorldpaySmbMerchant](merchantKey)
  }

  override def stringify(merchant: WorldpaySmbMerchant): String = {
    Serialization.write(merchant)
  }
}
