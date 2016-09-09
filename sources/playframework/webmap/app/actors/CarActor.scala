package actors

import akka.actor._

object CarActor {
  def props = Props[CarActor]
  
  case class GetPosition()
  case class CarData(name: String, latitude: Double, longitude: Double)
}

class CarActor extends Actor {
  import CarActor._

  var name = "<NO NAME>"
  var latitude = 0.0
  var longitude = 0.0
  
  def receive = {
    case GetPosition() => sender() ! Position(latitude, longitude)
    case CarData(n, lat, long) => {
      name = n
      latitude = lat
      longitude = long
    }
  }
}