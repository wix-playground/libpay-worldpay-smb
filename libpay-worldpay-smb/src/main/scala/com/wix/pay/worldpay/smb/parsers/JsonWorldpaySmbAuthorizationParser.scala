package com.wix.pay.worldpay.smb.parsers

import com.wix.pay.worldpay.smb.WorldpaySmbAuthorization
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

object JsonWorldpaySmbAuthorizationParser extends WorldpaySmbAuthorizationParser {
  private implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): WorldpaySmbAuthorization = {
    Serialization.read[WorldpaySmbAuthorization](authorizationKey)
  }

  override def stringify(authorization: WorldpaySmbAuthorization): String = {
    Serialization.write(authorization)
  }
}
