package com.wix.pay.worldpay.smb

import com.wix.pay.model.Deal
import com.worldpay.gateway.clearwater.client.core.dto.CurrencyCode
import com.worldpay.gateway.clearwater.client.core.dto.request.OrderRequest
import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class WorldpaySmbRequestBuilderTest extends SpecWithJUnit with WorldpayTestSupport {
  "WorldpaySmbRequestBuilder" should {
    "use default order description when deal is not available" in new Ctx {
      createOrderRequest(None) must haveDescription(WorldpaySmbRequestBuilder.defaultDealDescription)
    }

    "use deal description as order description if deal is available and has a description" in new Ctx {
      createOrderRequest(Some(someDeal)) must haveDescription(someDeal.description.get)
    }

    "use deal title as order description if deal is available, has no description but has a title" in new Ctx {
      createOrderRequest(Some(someDeal.withoutDescription)) must haveDescription(someDeal.title.get)
    }

    "use deal id as order description if deal is available but has no description or title" in new Ctx {
      createOrderRequest(Some(someDeal.withoutDescription.withoutTitle)) must haveDescription(someDeal.id)
    }

    "use merchant settlement currency" in new Ctx {
      createOrderRequest(None, merchant = someMerchant.copy(settlementCurrency = "UAH")) must haveSettlementCurrency(CurrencyCode.UAH)
    }

    "use exact worldPay amount for createCaptureRequest" in new Ctx {
      WorldpaySmbRequestBuilder.createCaptureRequest(145.20).getCaptureAmount must beEqualTo(14520)
    }
  }

  trait Ctx extends Scope {
    def createOrderRequest(deal: Option[Deal],
                           merchant: WorldpaySmbMerchant = someMerchant) = WorldpaySmbRequestBuilder.createOrderRequest(
      merchant, someCreditCard, someCurrencyAmount, deal, authorizeOnly = true
    )

    def haveDescription(description: String): Matcher[OrderRequest] = {
      be_===(description) ^^ { (_: OrderRequest).getOrderDescription }
    }

    def haveSettlementCurrency(currency: CurrencyCode): Matcher[OrderRequest] = {
      be_===(currency) ^^ { (_: OrderRequest).getSettlementCurrency }
    }
  }
}
