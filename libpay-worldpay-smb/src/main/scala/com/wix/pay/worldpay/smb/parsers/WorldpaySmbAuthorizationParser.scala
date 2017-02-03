package com.wix.pay.worldpay.smb.parsers

import com.wix.pay.worldpay.smb.WorldpaySmbAuthorization

trait WorldpaySmbAuthorizationParser {
  def parse(authorizationKey: String): WorldpaySmbAuthorization
  def stringify(authorization: WorldpaySmbAuthorization): String
}