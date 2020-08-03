package models

object Providers extends ModelCompanion[Providers] {
  protected def dbTable: DatabaseTable[Providers] = Database.providers

    def apply(username: String, locationId: Int, storeName: String, maxDeliveryDistance: Int): Providers =
      new Providers(username, locationId, storeName, maxDeliveryDistance)

    private[models] def apply(jsonValue: JValue): Providers = {
      val value = jsonValue.extract[Providers]
      value._id = (jsonValue \ "id").extract[Int]
      value
    }
  }

class Providers(val username: String, val locationId: Int, val storeName: String,
               val maxDeliveryDistance: Int) extends Model[Providers] {
  protected def dbTable: DatabaseTable[Providers] = Providers.dbTable

  private var _balance: Float = 0 

  override def toMap: Map[String, Any] = super.toMap + ("username" -> username, "locationId" -> locationId, 
                                                        "storeName" -> storeName,
                                                        "maxDeliveryDistance" -> maxDeliveryDistance,
                                                        "balance" -> _balance)

  def updateBalanceProvider(orderTotal: Float) : Unit = {
    _balance = _balance + orderTotal
  }

  override def toString: String = s"Providers: $username"
}
