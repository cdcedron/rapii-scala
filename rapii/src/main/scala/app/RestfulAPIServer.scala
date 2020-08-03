package app

import cask._
import models._
import java.util.function.Consumer
import scala.collection.script.Location
import java.security.Provider
import java.awt.ItemSelectable

object RestfulAPIServer extends MainRoutes  {
  override def host: String = "0.0.0.0"
  override def port: Int = 4000

  @get("/")
  def root(): Response = {
    JSONResponse("Ok")
  }

  // LOCATIONS
  @get("/api/locations")
  def locations(): Response = {
    JSONResponse(Location.all.map(location => location.toMap))
  }

  @postJson("/api/locations")
  def locations(name: String, coordX: Int, coordY: Int): Response = {
    if (Location.exists("name", name)) {
      return JSONResponse("Existing location", 409)
    }

    val location = Location(name, coordX, coordY)
    location.save()
    JSONResponse(location.id)
  }

  // CONSUMERS
  @get("/api/consumers")
  def consumers(username: String = "All_consumers"): Response = {
    if (username == "All_consumers") {
      return JSONResponse(Consumers.all.map(consumer => consumer.toMap))
    }

    JSONResponse(Consumers.filter(Map("username" -> username)).map(consumer => consumer.toMap))
  }

  @postJson("/api/consumers")
  def consumers(username: String, locationName: String): Response = {
    if (!Location.exists("name", locationName)) {
        return JSONResponse("Non existing location", 404)
    }
    if((Consumers.exists("username", username)) || (Providers.exists("username", username))) {
      return JSONResponse("Existing username", 409)
    }

    val locationId : Int = Location.filter(Map("name" -> locationName)).head.id
    val consumer = Consumers(username, locationId)
    consumer.save()
    JSONResponse(consumer.id)
  }

  @post("/api/consumers/delete")
  def consumersDelete(id: Int): Response = {
    if (!Consumers.exists("id", id)) {
        return JSONResponse("Non existing consumer ", 404)
    }

    Consumers.delete(id)
    JSONResponse("OK")
  }

  // PROVIDERS
  @get("/api/providers")
  def providers(locationName: String = "All_Providers") : Response = {
    if (locationName == "All_Providers") {
      return JSONResponse(Providers.all.map(provider => provider.toMap))
    }
    if (!Location.exists("name", locationName)) {
      return JSONResponse("Non existing location", 404)
    }

    val locationId : Int = Location.filter(Map("name" -> locationName)).head.id
    JSONResponse(Providers.filter(Map("locationId" -> locationId)).map(provider => provider.toMap))

 }

  @postJson("/api/providers")
  def providers(username: String, storeName: String, locationName: String, 
                maxDeliveryDistance: Int): Response = {
    
    if((Consumers.exists("username", username)) || 
      (Providers.exists("username", username)) || 
      (Providers.exists("storeName", storeName))) {
      return JSONResponse("Existing username/storeName", 409)
    }
    if (maxDeliveryDistance < 0) {
      return JSONResponse("Negative maxDeliveryDistance", 400)
    }
    if (!Location.exists("name", locationName)) {
      return JSONResponse("Non existing location", 404)
    }

    val locationId: Int = Location.filter(Map("name" -> locationName)).head.id
    val provider = Providers(username, locationId, storeName, maxDeliveryDistance)
    provider.save()
    JSONResponse(provider.id)
  }

  @post("/api/providers/delete")
  def providersDelete(id: Int): Response = {
    if (!Providers.exists("id", id)) {
        return JSONResponse("Non existing provider", 404)
    }

    Providers.delete(id)
    JSONResponse("OK")
  }

  //ITEMS
  @get("/api/items")
  def items(providerUsername: String = "All_Items"): Response = {
    if (providerUsername == "All_Items") {
      return JSONResponse(Items.all.map(item => item.toMap))
    }
    if (!Providers.exists("username", providerUsername)) {
      return JSONResponse("Non existing provider", 404)
    }

    val providerId : Int = Providers.filter(Map("username" -> providerUsername)).head.id
    JSONResponse(Items.filter(Map("providerId" -> providerId)).map(item => item.toMap))
  }

  @postJson("/api/items")
  def items(name: String, description: String, price: Float, providerUsername: String): Response = {
    if (price < 0) {
      return JSONResponse("Negative price", 400)
    }
    if (!Providers.exists("username", providerUsername)) {
      return JSONResponse("Non existing provider", 404)
    }

    val providerId: Int = Providers.filter(Map("username" -> providerUsername)).head.id
 
    if(!(Items.filter(Map(("name" ->  name), ("providerId" -> providerId)))).isEmpty) {
      return JSONResponse("Existing item for provider", 409)
    }

    val items = Items(name, description, price, providerId)
    items.save()
    JSONResponse(items.id)
  }

  @post("/api/items/delete")
  def itemDelete(id: Int): Response = {
    if (!Items.exists("id", id)) {
        return JSONResponse("Non existing item ", 404)
    }

    Items.delete(id)
    JSONResponse("OK")
  }

  // ORDERS
  @get("/api/orders")
  def orders(username: String): Response = {
    if ((!Consumers.exists("username", username)) && (!Providers.exists("username", username)))  {
      return JSONResponse("Non existing user", 404)
    }
    if (Consumers.exists("username", username)) {
      val consumerId : Int = Consumers.filter(Map("username" -> username)).head.id
      return JSONResponse(Orders.filter(Map("consumerId" -> consumerId)).map(item => item.toMap))
    }
    val providerId : Int = Providers.filter(Map("username" -> username)).head.id
    return JSONResponse(Orders.filter(Map("providerId" -> providerId)).map(item => item.toMap))
  }

  @get("/api/orders/detail")
  def orderDetail(id: Int): Response = {
    if (!Orders.exists("id", id)) {
        return JSONResponse("Non existing order", 404)
    }
    
     JSONResponse(Orders.find(id))
  } 

  @postJson("/api/orders")
  def orders(providerUsername: String, consumerUsername: String, items: List[Thing]): Response = {
    if (negativeAmount(items)) { 
      return JSONResponse("Negative amount", 400)
    }
    if((!Providers.exists("username", providerUsername)) || 
      (!Consumers.exists("username", consumerUsername)) ||
      (existItems(items))) {
      return JSONResponse("non existing consumer/provider/items", 404)
    }

  def existItems (items: List[Thing]): Boolean = {
    var n: Int = items.length
    var res: Boolean = true
    for (i <- 0 until n) {
      var temp: Boolean = !Items.exists("name", items(i).name)
      res = res && temp
    }
    return res
  }

  def getOrderTotal(items: List[Thing]): Float = {
    var n: Int = items.length
    var res: Float = 0
    for (i <- 0 until n) {
      var temp: Float = Items.filter(Map("name" -> items(i).name)).head.price * items(i).amount
      res = res + temp
    }
  return res
  }

  def negativeAmount(items: List[Thing]): Boolean = {
    var n: Int = items.length
    var res: Boolean = false
    for (i <- 0 until n) {
      var temp: Boolean = (items(i).amount < 0)
      res = res || temp
    }
  return res
  }

  val consumerId : Int = Consumers.filter(Map("username" -> consumerUsername)).head.id
  val locationId: Int = Consumers.find(consumerId).get.locationId
  val consumerLocation : String = Location.filter(Map("id" -> locationId)).head.name
  val providerId : Int = Providers.filter(Map("username" -> providerUsername)).head.id
  val providerStoreName : String = Providers.filter(Map("id" -> providerId)).head.storeName
  var orderTotal: Float = getOrderTotal(items)
        
  def haveItems (items: List[Thing]): Boolean = {
    var n: Int = items.length
    var res: Boolean = true      
    for (i <- 0 until n) {
      var temp: Boolean = (!(Items.filter(Map(("name" ->  items(i).name), ("providerId" -> providerId)))).isEmpty)
      res = res && temp
    }
  return res
  }

  if(!haveItems(items)) {
    return JSONResponse("non existing item for provider", 404)
  }
  
  //Update Balance
  Consumers.find(consumerId).get.updateBalanceConsumer(orderTotal)
  Providers.find(providerId).get.updateBalanceProvider(orderTotal)

  val orders = Orders(consumerId, consumerUsername, consumerLocation, 
                      providerId, providerStoreName, orderTotal, items)
  orders.save()
  JSONResponse(orders.id)
  }

  @post("/api/orders/delete")
  def ordersDelete(id: Int): Response = {
    if (!Orders.exists("id", id)) {
        return JSONResponse("non existing order ", 404)
    }

    Orders.delete(id)
    JSONResponse("OK")
  }

  @post("/api/orders/deliver")
  def ordersDeliver(id: Int): Response = {
    if (!Orders.exists("id", id)) {
      return JSONResponse("non existing order ", 404)
    }
    if (Orders.find(id).get.checkStatus()) {
      return JSONResponse("Already Delivered", 409)
    }

    Orders.find(id).get.deliver()
    JSONResponse("OK")
  }


  override def main(args: Array[String]): Unit = {
    System.err.println("\n " + "=" * 39)
    System.err.println(s"| Server running at http://$host:$port ")

    if (args.length > 0) {
      val databaseDir = args(0)
      Database.loadDatabase(databaseDir)
      System.err.println(s"| Using database directory $databaseDir ")
    } else {
      Database.loadDatabase()  // Use default location
    }
    System.err.println(" " + "=" * 39 + "\n")

    super.main(args)
  }

  initialize()
}
