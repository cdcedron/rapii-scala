package models

import upickle.default.{ReadWriter => RW, macroRW}
import upickle.default._

case class Thing(name: String, amount: Int)
object Thing{
  implicit val rw: RW[Thing] = macroRW
}

object Orders extends ModelCompanion[Orders] {
  protected def dbTable: DatabaseTable[Orders] = Database.orders

   def apply(consumerId: Int, consumerUsername: String, consumerLocation: String, 
             providerId: Int, providerStoreName: String,
             orderTotal: Float, items: List[Thing]): Orders =
     new Orders(consumerId, consumerUsername, consumerLocation, providerId, 
                providerStoreName, orderTotal, items)

   private[models] def apply(jsonValue: JValue): Orders = {
     val value = jsonValue.extract[Orders]
     value._id = (jsonValue \ "id").extract[Int]
     value
   }
 }

class Orders(val consumerId: Int, val consumerUsername: String, val consumerLocation: String, 
             val providerId: Int, val providerStoreName: String,
             val orderTotal: Float, val items: List[Thing]) extends Model[Orders] {
  
  private var _status: String = "payed"

  protected def dbTable: DatabaseTable[Orders] = Orders.dbTable

  override def toMap: Map[String, Any] = super.toMap + ("consumerId" -> consumerId, 
                                                        "consumerUsername" -> consumerUsername,
                                                        "consumerLocation" -> consumerLocation,
                                                        "providerId" -> providerId,
                                                        "providerStoreName" -> providerStoreName,
                                                        "orderTotal" -> orderTotal,
                                                        "status" -> _status,
                                                        "items" -> items)

  def deliver(): Unit = {
    _status = "delivered"
  }

  def checkStatus(): Boolean = {
    return (_status == "delivered")
  }

  override def toString: String = s"Orders: $consumerUsername"
}
