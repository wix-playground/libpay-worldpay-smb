package com.wix.pay.worldpay.smb.testkit

import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Deal}
import org.json4s.DefaultFormats
import org.json4s.native.Serialization
import spray.http._

class WorldpaySmbDriver(port: Int) {
  private val worldpayProbe = new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler)

  def reset(): Unit = worldpayProbe.reset()

  def start(): Unit = worldpayProbe.doStart()

  def stop(): Unit = worldpayProbe.doStop()

  def requests = worldpayProbe.requests

  def anAuthorizationRequest(serviceKey: String,
                             creditCard: CreditCard,
                             currencyAmount: CurrencyAmount,
                             deal: Option[Deal]) = {
    AuthorizationRequest(serviceKey, creditCard, currencyAmount, deal, authorizeOnly = true)
  }

  def aCaptureRequest(serviceKey: String,
                      orderCode: String,
                      creditCard: CreditCard,
                      currencyAmount: CurrencyAmount,
                      deal: Option[Deal]) = {
    CaptureRequest(serviceKey, orderCode, creditCard, currencyAmount, deal)
  }

  def aSaleRequest(serviceKey: String,
                   creditCard: CreditCard,
                   currencyAmount: CurrencyAmount,
                   deal: Option[Deal]) = {
    AuthorizationRequest(serviceKey, creditCard, currencyAmount, deal, authorizeOnly = false)
  }

  def aVoidAuthorizationRequest(serviceKey: String,
                                orderCode: String) = {
    VoidAuthorizationRequest(serviceKey, orderCode)
  }

  abstract class WorldpayRequest(serviceKey: String, path: String, method: HttpMethod = HttpMethods.POST) {
    protected def expectedJsonBody: Map[String, Any]
    protected def validResponse(orderCode: String): Map[String, Any]

    def returns(orderCode: String): Unit = {
      val response = removeEmptyValuesFromMap(validResponse(orderCode))
      respondWith(StatusCodes.OK, toJson(response))
    }

    def isAnErrorWith(statusCode: StatusCode, errorDescription: String): Unit = {
      val response = Map(
        "httpStatusCode" -> statusCode.intValue,
        "customCode" -> errorDescription,
        "message" -> errorDescription,
        "description" -> errorDescription
      )
      respondWith(statusCode, toJson(response))
    }

    protected def respondWith(status: StatusCode, content: String): Unit = {
      worldpayProbe.handlers += {
        case HttpRequest(requestMethod, requestPath, headers, entity, _)
          if requestMethod == method &&
            requestPath.path == Uri(path).path &&
            isJson(headers) &&
            isAuthorized(headers) &&
            isStubbedEntity(entity) =>
          HttpResponse(status = status, entity = content)
      }
    }

    private def isJson(headers: List[HttpHeader]): Boolean = headers.exists { header =>
      header.name == "Content-Type" && header.value == "application/json"
    }

    private def isAuthorized(headers: List[HttpHeader]): Boolean = headers.exists { header =>
      header.name == "Authorization" && header.value == serviceKey
    }

    protected def isStubbedEntity(entity: HttpEntity): Boolean = {
      val actual = toMap(entity)
      val expected = removeEmptyValuesFromMap(expectedJsonBody)
      actual == expected
    }

    implicit val formats = DefaultFormats
    protected def toJson(map: Map[String, Any]): String = Serialization.write(map)
    private def toMap(entity: HttpEntity): Map[String, Any] = Serialization.read[Map[String, Any]](entity.asString)

    protected def removeEmptyValuesFromMap(map: Map[String, Any]): Map[String, Any] = map.flatMap { case (key, value) =>
      val filteredValue = filterValue(value)
      filteredValue.map(key -> _)
    }

    private def filterValue(value: Any): Option[Any] = value match {
      case str: String => if (str.nonEmpty) Some(str) else None
      case Some(o) => filterValue(o)
      case None => None
      case map: Map[String, Any] =>
        val filteredMap = removeEmptyValuesFromMap(map)
        if (filteredMap.nonEmpty) Some(filteredMap) else None
      case x => Some(x)
    }

    protected def toWorldPayAmount(currencyAmount: CurrencyAmount): Int = (currencyAmount.amount * 100).toInt
  }

  case class AuthorizationRequest(serviceKey: String,
                                  creditCard: CreditCard,
                                  currencyAmount: CurrencyAmount,
                                  deal: Option[Deal],
                                  authorizeOnly: Boolean)
    extends WorldpayRequest(serviceKey, path = "/orders") with WorldpayHelper {

    override protected def expectedJsonBody = Map(
      "authorizeOnly" -> authorizeOnly,
      "paymentMethod" -> Map(
        "type" -> "Card",
        "name" -> creditCard.holderName.get,
        "expiryMonth" -> creditCard.expiration.month,
        "expiryYear" -> creditCard.expiration.year,
        "cardNumber" -> creditCard.number,
        "cvc" -> creditCard.csc.get
      ),
      "reusable" -> false,
      "orderDescription" -> deal.flatMap(_.description),
      "amount" -> toWorldPayAmount(currencyAmount),
      "currencyCode" -> currencyAmount.currency,
      "billingAddress" -> billingAddressMap,
      "deliveryAddress" -> shippingAddressMap,
      "name" -> creditCard.holderName.get,
      "customerOrderCode" -> deal.flatMap(_.invoiceId)
    )

    def isRejectedWith(orderCode: String, reason: String): Unit = {
      val response = removeEmptyValuesFromMap(rejectResponse(orderCode, reason))
      respondWith(StatusCodes.OK, toJson(response))
    }

    private def rejectResponse(orderCode: String, reason: String): Map[String, Any] =
      response(orderCode, paymentStatus = "FAILED", cvcStatus = "FAILED", statusReason = Some(reason), authorizeOnly)

    override protected def validResponse(orderCode: String) =
      response(orderCode, paymentStatus = "AUTHORIZED", cvcStatus = "APPROVED", statusReason = None, authorizeOnly)
  }

  case class CaptureRequest(serviceKey: String,
                            orderCode: String,
                            creditCard: CreditCard,
                            currencyAmount: CurrencyAmount,
                            deal: Option[Deal])
    extends WorldpayRequest(serviceKey, path = s"/orders/$orderCode/capture") with WorldpayHelper {

    override protected def expectedJsonBody = Map(
      "captureAmount" -> toWorldPayAmount(currencyAmount),
      "merchantId" -> null
    )

    protected override def validResponse(orderCode: String) =
      response(orderCode, paymentStatus = "SUCCESS", cvcStatus = "APPROVED", statusReason = None, authorizeOnly = true)
  }

  case class VoidAuthorizationRequest(serviceKey: String,
                                      orderCode: String) extends WorldpayRequest(serviceKey, path = s"/orders/$orderCode", method = HttpMethods.DELETE) {
    override protected def expectedJsonBody = Map.empty
    override protected def isStubbedEntity(entity: HttpEntity) = true
    protected override def validResponse(orderCode: String) = Map.empty
  }

  trait WorldpayHelper { self: WorldpayRequest =>
    def creditCard: CreditCard
    def currencyAmount: CurrencyAmount
    def deal: Option[Deal]

    private val billingAddress = creditCard.billingAddressDetailed
    protected val billingAddressMap = Map(
      "address1" -> billingAddress.flatMap(_.street),
      "postalCode" -> billingAddress.flatMap(_.postalCode),
      "city" -> billingAddress.flatMap(_.city),
      "state" -> billingAddress.flatMap(_.state),
      "countryCode" -> billingAddress.flatMap(_.countryCode).map(_.getCountry.toUpperCase)
    )

    private val shippingAddress = deal.flatMap(_.shippingAddress)
    protected val shippingAddressMap = Map(
      "address1" -> shippingAddress.flatMap(_.street),
      "postalCode" -> shippingAddress.flatMap(_.postalCode),
      "city" -> shippingAddress.flatMap(_.city),
      "state" -> shippingAddress.flatMap(_.state),
      "countryCode" -> shippingAddress.flatMap(_.countryCode).map(_.getCountry.toUpperCase),
      "phone" -> shippingAddress.flatMap(_.phone),
      "firstName" -> shippingAddress.flatMap(_.firstName),
      "lastName" -> shippingAddress.flatMap(_.lastName)
    )

    protected def response(orderCode: String,
                           paymentStatus: String,
                           cvcStatus: String,
                           statusReason: Option[String],
                           authorizeOnly: Boolean) = Map(
      "orderCode" -> orderCode,
      "token" -> "someToken",
      "orderDescription" -> deal.flatMap(_.description),
      "amount" -> toWorldPayAmount(currencyAmount),
      "currencyCode" -> currencyAmount.currency,
      "paymentStatus" -> paymentStatus,
      "paymentStatusReason" -> statusReason,
      "paymentResponse" -> Map(
        "type" -> "ObfuscatedCard",
        "name" -> creditCard.holderName.get,
        "expiryMonth" -> creditCard.expiration.month,
        "expiryYear" -> creditCard.expiration.year,
        "cardType" -> "VISA_CREDIT",
        "maskedCardNumber" -> "**** **** **** 1111",
        "billingAddress" -> billingAddressMap,
        "cardSchemeType" -> "consumer",
        "cardSchemeName" -> "VISA CREDIT",
        "cardIssuer" -> "NATWEST",
        "countryCode" -> billingAddress.flatMap(_.countryCode).map(_.getCountry),
        "cardClass" -> "credit",
        "cardProductTypeDescNonContactless" -> "Visa Credit Personal",
        "cardProductTypeDescContactless" -> "CL Visa Credit Pers",
        "prepaid" -> "false"
      ),
      "authorizeOnly" -> authorizeOnly,
      "deliveryAddress" -> shippingAddressMap,
      "customerOrderCode" -> deal.flatMap(_.invoiceId),
      "environment" -> "TEST",
      "authorizedAmount" -> toWorldPayAmount(currencyAmount),
      "riskScore" -> Map(
        "value" -> "1"
      ),
      "resultCodes" -> Map(
        "avsResultCode" -> "APPROVED",
        "cvcResultCode" -> cvcStatus
      )
    )
  }
}
