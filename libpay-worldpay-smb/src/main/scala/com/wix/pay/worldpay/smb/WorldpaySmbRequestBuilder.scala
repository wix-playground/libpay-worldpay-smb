package com.wix.pay.worldpay.smb

import com.wix.pay.creditcard.{AddressDetailed, CreditCard}
import com.wix.pay.model.{CurrencyAmount, Deal, ShippingAddress}
import com.worldpay.gateway.clearwater.client.core.dto.{CountryCode, CurrencyCode}
import com.worldpay.gateway.clearwater.client.core.dto.common.{Address, CommonToken, DeliveryAddress}
import com.worldpay.gateway.clearwater.client.core.dto.request.{CaptureOrderRequest, CardRequest, OrderRequest}

object WorldpaySmbRequestBuilder {
  val defaultDealDescription = "Description unavailable"

  def createOrderRequest(creditCard: CreditCard, currencyAmount: CurrencyAmount, deal: Option[Deal], authorizeOnly: Boolean): OrderRequest = {
    val orderRequest = new OrderRequest()
    orderRequest.setAuthorizeOnly(authorizeOnly)
    orderRequest.setAmount(toWorldpayAmount(currencyAmount))
    orderRequest.setCurrencyCode(CurrencyCode.valueOf(currencyAmount.currency))
    orderRequest.setName(creditCard.holderName.get)

    val cardRequest = createCardRequest(creditCard)
    orderRequest.setCommonToken(new CommonToken(cardRequest, false))

    for (billingAddress <- creditCard.billingAddressDetailed) fillBillingAddress(orderRequest, billingAddress)
    fillDeal(orderRequest, deal)

    orderRequest
  }

  def createCaptureRequest(amount: Double): CaptureOrderRequest = {
    new CaptureOrderRequest(toWorldpayAmount(amount))
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

  private def fillDeal(orderRequest: OrderRequest, deal: Option[Deal]): Unit = {
    val description = deal.map { deal =>
      deal.description.getOrElse(deal.title.getOrElse(deal.id))
    }.getOrElse(defaultDealDescription)
    orderRequest.setOrderDescription(description)

    for (deal <- deal) {
      for (invoiceId <- deal.invoiceId) orderRequest.setCustomerOrderCode(invoiceId)
      for (shippingAddress <- deal.shippingAddress) fillShippingAddress(orderRequest, shippingAddress)
    }
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
}
