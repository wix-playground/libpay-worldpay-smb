package com.wix.pay.worldpay.smb

import com.wix.pay.creditcard.{AddressDetailed, CreditCard}
import com.wix.pay.model._
import com.wix.pay.worldpay.smb.parsers.{JsonWorldpaySmbAuthorizationParser, JsonWorldpaySmbMerchantParser, WorldpaySmbAuthorizationParser, WorldpaySmbMerchantParser}
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}
import com.worldpay.api.client.common.enums.OrderStatus
import com.worldpay.gateway.clearwater.client.core.dto.common.{Address, CommonToken, DeliveryAddress}
import com.worldpay.gateway.clearwater.client.core.dto.request.{CaptureOrderRequest, CardRequest, OrderRequest}
import com.worldpay.gateway.clearwater.client.core.dto.{CountryCode, CurrencyCode}
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
      val request = new CaptureOrderRequest(toWorldpayAmount(amount))
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

      val client = createClient(merchantKey)

      val orderRequest = createOrderRequest(creditCard, payment.currencyAmount, deal, authorizeOnly)
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

  private def createOrderRequest(creditCard: CreditCard, currencyAmount: CurrencyAmount, deal: Option[Deal], authorizeOnly: Boolean): OrderRequest = {
    val orderRequest = new OrderRequest()
    orderRequest.setAuthorizeOnly(authorizeOnly)
    orderRequest.setAmount(toWorldpayAmount(currencyAmount))
    orderRequest.setCurrencyCode(CurrencyCode.valueOf(currencyAmount.currency))
    orderRequest.setName(creditCard.holderName.get)

    val cardRequest = createCardRequest(creditCard)
    orderRequest.setCommonToken(new CommonToken(cardRequest, false))

    for (billingAddress <- creditCard.billingAddressDetailed) fillBillingAddress(orderRequest, billingAddress)
    for (deal <- deal) fillDeal(orderRequest, deal)

    orderRequest
  }

  private def createCardRequest(creditCard: CreditCard): CardRequest = {
    val card = new CardRequest()
    card.setCardNumber(creditCard.number)
    card.setCvc(creditCard.csc.get)
    card.setExpiryMonth(creditCard.expiration.month)
    card.setExpiryYear(creditCard.expiration.year)
    card.setName(creditCard.holderName.get)
    card
  }

  private def fillBillingAddress(orderRequest: OrderRequest, billingAddress: AddressDetailed): Unit = {
    val address = new Address()
    fillAddress(address, billingAddress)
    orderRequest.setBillingAddress(address)
  }

  private def fillDeal(orderRequest: OrderRequest, deal: Deal): Unit = {
    for (description <- deal.description) orderRequest.setOrderDescription(description)
    for (invoiceId <- deal.invoiceId) orderRequest.setCustomerOrderCode(invoiceId)
    for (shippingAddress <- deal.shippingAddress) fillShippingAddress(orderRequest, shippingAddress)
  }

  private def fillShippingAddress(orderRequest: OrderRequest, shippingAddress: ShippingAddress): Unit = {
    val address = new DeliveryAddress(shippingAddress.firstName.getOrElse(""), shippingAddress.lastName.getOrElse(""))
    for (details <- shippingAddress.address) fillAddress(address, details)
    for (phone <- shippingAddress.phone) address.setTelephoneNumber(phone)
    orderRequest.setDeliveryAddress(address)
  }

  private def fillAddress(address: Address, details: AddressDetailed): Unit = {
    for (street <- details.street) address.setAddress1(street)
    for (city <- details.city) address.setCity(city)
    for (state <- details.state) address.setState(state)
    for (postalCode <- details.postalCode) address.setPostalCode(postalCode)
    for (countryCode <- details.countryCode) address.setCountryCode(CountryCode.fromValue(countryCode.getCountry))
  }

  private def toWorldpayAmount(currencyAmount: CurrencyAmount): Int = toWorldpayAmount(currencyAmount.amount)
  private def toWorldpayAmount(amount: Double): Int = (amount * 100).toInt

  private def withOrderCode(merchantKey: String,
                            authorizationKey: String)
                           (f: (WorldpayRestClient, String) => Unit): Try[String] = {
    withExceptionHandling {
      val client = createClient(merchantKey)
      val authorization = authorizationParser.parse(authorizationKey)
      val orderCode = authorization.orderCode
      f(client, orderCode)
      orderCode
    }
  }

  private def createClient(merchantKey: String) = {
    val merchant = merchantParser.parse(merchantKey)
    new WorldpayRestClient(endpointUrl, merchant.serviceKey)
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
