package models
import java.util.function.Consumer

object Consumers extends ModelCompanion[Consumers] {
  protected def dbTable: DatabaseTable[Consumers] = Database.consumers

  def apply(username: String, locationId: Int): Consumers =
    new Consumers(username, locationId)

  private[models] def apply(jsonValue: JValue): Consumers = {
    val value = jsonValue.extract[Consumers]
    value._id = (jsonValue \ "id").extract[Int]
    value
  }
}

class Consumers(val username: String, val locationId: Int) extends Model[Consumers] {
  protected def dbTable: DatabaseTable[Consumers] = Consumers.dbTable

  private var _balance: Float = 0 

  override def toMap: Map[String, Any] = super.toMap + ("username" -> username, "locationId" -> locationId, 
                                                        "balance" -> _balance)
  
  
  def updateBalanceConsumer(orderTotal: Float) : Unit = {
    _balance = _balance - orderTotal
  }

  override def toString: String = s"Consumers: $username"
}