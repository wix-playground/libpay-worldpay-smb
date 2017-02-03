package com.wix.pay.worldpay.smb.parsers

import com.wix.pay.worldpay.smb.WorldpaySmbMerchant

trait WorldpaySmbMerchantParser {
  def parse(merchantKey: String): WorldpaySmbMerchant
  def stringify(merchant: WorldpaySmbMerchant): String
}
