package models

object Items extends ModelCompanion[Items] {
  protected def dbTable: DatabaseTable[Items] = Database.items

   def apply(name: String, description: String, price: Float, providerId: Int): Items =
     new Items(name, description, price, providerId)

   private[models] def apply(jsonValue: JValue): Items = {
     val value = jsonValue.extract[Items]
     value._id = (jsonValue \ "id").extract[Int]
     value
   }
 }

class Items(val name: String, val description: String, val price: Float,
            val providerId: Int) extends Model[Items] {
  protected def dbTable: DatabaseTable[Items] = Items.dbTable

  override def toMap: Map[String, Any] = super.toMap + ("name" -> name, "description" -> description,
                                                        "price" -> price, "providerId" -> providerId)

  override def toString: String = s"Items: $name"
}
