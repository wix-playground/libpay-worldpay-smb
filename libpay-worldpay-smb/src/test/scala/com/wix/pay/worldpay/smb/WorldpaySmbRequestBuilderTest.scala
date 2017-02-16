package com.wix.pay.worldpay.smb

import com.wix.pay.model.Deal
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
      createOrderRequest(Some(deal)) must haveDescription(deal.description.get)
    }

    "use deal title as order description if deal is available, has no description but has a title" in new Ctx {
      createOrderRequest(Some(deal.withoutDescription)) must haveDescription(deal.title.get)
    }

    "use deal id as order description if deal is available but has no description or title" in new Ctx {
      createOrderRequest(Some(deal.withoutDescription.withoutTitle)) must haveDescription(deal.id)
    }
  }

  trait Ctx extends Scope {
    def createOrderRequest(deal: Option[Deal]) = WorldpaySmbRequestBuilder.createOrderRequest(creditCard, currencyAmount, deal, authorizeOnly = true)

    def haveDescription(description: String): Matcher[OrderRequest] = {
      be_===(description) ^^ { (_: OrderRequest).getOrderDescription }
    }
  }
}