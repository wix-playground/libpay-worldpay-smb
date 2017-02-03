package com.wix.pay.worldpay.smb

import com.worldpay.gateway.clearwater.client.core.dto.common.{Address, CommonToken, DeliveryAddress}
import com.worldpay.gateway.clearwater.client.core.dto.request.{CaptureOrderRequest, CardRequest, OrderRequest}
import com.worldpay.gateway.clearwater.client.core.dto.{CountryCode, CurrencyCode}
import com.worldpay.gateway.clearwater.client.core.exception.WorldpayException
import com.worldpay.sdk.WorldpayRestClient

/**
  * An application that sends requests to Worldpay. Can be used to test and/or record Worldpay requests.
  * Uncomment the methods you want to get called {authorize, capture, sale, void} and run.
  */
object WorldpaySmbFakeClient extends App {
  val orderRequest = {
    val orderRequest = new OrderRequest()

    val billingAddress = new Address()
    billingAddress.setAddress1("123 House Road")
    billingAddress.setCity("London")
    billingAddress.setCountryCode(CountryCode.GB)
    billingAddress.setPostalCode("EC1 1AA")
    orderRequest.setBillingAddress(billingAddress)

    val shippingAddress = new DeliveryAddress("John", "Smith")
    shippingAddress.setAddress1("123 House Road2")
    shippingAddress.setCity("London2")
    shippingAddress.setCountryCode(CountryCode.DE)
    shippingAddress.setPostalCode("EC1 1AA2")
    orderRequest.setDeliveryAddress(shippingAddress)

    orderRequest.setAmount(10)
    orderRequest.setCurrencyCode(CurrencyCode.GBP)
    orderRequest.setName("test name")
    orderRequest.setOrderDescription("Order description")
    orderRequest.setCustomerOrderCode("Order code")

    val card = new CardRequest()
    card.setCardNumber("4444333322221111")
    card.setCvc("123")
    card.setExpiryMonth(12)
    card.setExpiryYear(2020)
    card.setName("SUCCESS")

    orderRequest.setCommonToken(new CommonToken(card, false))
    orderRequest
  }

  // Replace with your key
  val serviceKey = "myServiceKey"

//  val client = new WorldpayRestClient("http://localhost:8080", serviceKey)
  val client = new WorldpayRestClient(serviceKey)


  // Uncomment the methods you want to get called
  val orderCode = authorize()
  capture(orderCode)
//  sale()
//  void(orderCode)

  def authorize(): String = {
    orderRequest.setAuthorizeOnly(true)

    withExceptionHandling {
      val orderResponse = client.getOrderService.create(orderRequest)
      println("Order code: " + orderResponse.getOrderCode)
      orderResponse.getOrderCode
    }
  }

  def capture(orderCode: String): Unit = {
    val request = new CaptureOrderRequest()
    request.setCaptureAmount(10)

    withExceptionHandling {
      val orderResponse = client.getOrderService.capture(request, orderCode)
      println("Order code: " + orderResponse.getOrderCode)
    }
  }

  def sale(): String = {
    orderRequest.setAuthorizeOnly(false)

    withExceptionHandling {
      val orderResponse = client.getOrderService.create(orderRequest)
      println("Order code: " + orderResponse.getOrderCode)
      orderResponse.getOrderCode
    }
  }

  def void(orderCode: String): Unit = {
    withExceptionHandling {
      client.getOrderService.cancel(orderCode)
    }
  }

  private def withExceptionHandling[T](f: => T): T = {
    try {
      f
    } catch {
      case e: WorldpayException =>
        e.printStackTrace()
        val error = e.getApiError
        println("Error code: " + error.getCustomCode)
        println("Error description: " + error.getDescription)
        println("Error message: " + error.getMessage)
        throw e
    }
  }
}
