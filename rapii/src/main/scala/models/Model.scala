package models

trait ModelCompanion[M <: Model[M]] {
  protected def dbTable: DatabaseTable[M]

  private[models] def apply(jsonValue: JValue): M

  def all: List[M] = dbTable.instances.values.toList

  def find(id: Int): Option[M] = dbTable.instances.get(id)

  def exists(attr: String, value: Any): Boolean = {
   
    def hasValue(m : M): Boolean = m.toMap.get(attr) == Some(value)

    dbTable.instances.exists(im => hasValue(im._2))
  }

  def delete(id: Int): Unit = dbTable.delete(id)

  def filter(mapOfAttributes: Map[String, Any]): List[M] = {
    
    def isOk(m : M): Boolean = mapOfAttributes.toSet subsetOf m.toMap.toSet 

    dbTable.instances.filter(im => isOk(im._2)).values.toList
  }
}

trait Model[M <: Model[M]] { self: M =>
  protected var _id: Int = 0

  def id: Int = _id

  protected def dbTable: DatabaseTable[M]

  def toMap: Map[String, Any] = Map("id" -> _id)

  def save(): Unit = {
    if (_id == 0) { _id = dbTable.getNextId }
    dbTable.save(this)
  }
}
