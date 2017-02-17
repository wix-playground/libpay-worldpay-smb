package com.wix.pay.worldpay.smb

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model._
import com.wix.pay.worldpay.smb.parsers.{JsonWorldpaySmbAuthorizationParser, JsonWorldpaySmbMerchantParser, WorldpaySmbAuthorizationParser, WorldpaySmbMerchantParser}
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}
import com.worldpay.api.client.common.enums.OrderStatus
import com.worldpay.gateway.clearwater.client.core.exception.WorldpayException
import com.worldpay.sdk.WorldpayRestClient

import scala.util.Try

class WorldpaySmbGateway(endpointUrl: String,
                         merchantParser: WorldpaySmbMerchantParser = JsonWorldpaySmbMerchantParser,
                         authorizationParser: WorldpaySmbAuthorizationParser = JsonWorldpaySmbAuthorizationParser) extends PaymentGateway {

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    for (orderCode <- submitOrder(merchantKey, creditCard, payment, deal, authorizeOnly = true)) yield {
      authorizationParser.stringify(WorldpaySmbAuthorization(orderCode))
    }
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    withOrderCode(merchantKey, authorizationKey) { (client, orderCode) =>
      val request = WorldpaySmbRequestBuilder.createCaptureRequest(amount)
      client.getOrderService.capture(request, orderCode)
    }
  }

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    submitOrder(merchantKey, creditCard, payment, deal, authorizeOnly = false)
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    withOrderCode(merchantKey, authorizationKey) { (client, orderCode) =>
      client.getOrderService.cancel(orderCode)
    }
  }

  private def submitOrder(merchantKey: String,
                          creditCard: CreditCard,
                          payment: Payment,
                          deal: Option[Deal],
                          authorizeOnly: Boolean): Try[String] = {
    withExceptionHandling {
      require(payment.installments == 1, "Worldpay does not support installments!")
      verifyRequiredParams(creditCard)

      val merchant = merchantParser.parse(merchantKey)
      val client = new WorldpayRestClient(endpointUrl, merchant.serviceKey)

      val orderRequest = WorldpaySmbRequestBuilder.createOrderRequest(merchant, creditCard, payment.currencyAmount, deal, authorizeOnly)
      val orderResponse = client.getOrderService.create(orderRequest)
      if (orderResponse.getPaymentStatus == OrderStatus.FAILED.name()) {
        throw new PaymentRejectedException(orderResponse.getPaymentStatusReason, cause = null)
      }
      orderResponse.getOrderCode
    }
  }

  private def verifyRequiredParams(creditCard: CreditCard): Unit = {
    require(creditCard.csc.isDefined, "Credit Card CSC is mandatory for Worldpay!")
    require(creditCard.holderName.isDefined, "Credit Card Holder Name is mandatory for Worldpay!")
  }

  private def withOrderCode(merchantKey: String,
                            authorizationKey: String)
                           (f: (WorldpayRestClient, String) => Unit): Try[String] = {
    withExceptionHandling {
      val merchant = merchantParser.parse(merchantKey)
      val client = new WorldpayRestClient(endpointUrl, merchant.serviceKey)
      val authorization = authorizationParser.parse(authorizationKey)
      val orderCode = authorization.orderCode
      f(client, orderCode)
      orderCode
    }
  }

  private def withExceptionHandling[T](f: => T): Try[T] = {
    Try {
      f
    } recover {
      case e: PaymentRejectedException => throw e
      case e: WorldpayException if e.getApiError.getHttpStatusCode == 400 => throw PaymentRejectedException(e.getMessage, e)
      case e => throw PaymentErrorException(e.getMessage, e)
    }
  }
}
