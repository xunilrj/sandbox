package actors

import akka.actor._

object CarActor {
  def props = Props[CarActor]
  
  case class GetData()
  case class CarData(name: Option[String], latitude: Double, longitude: Double)
}

class CarActor extends Actor {
  import CarActor._

  var name = "<NO NAME>"
  var latitude = 0.0
  var longitude = 0.0
  var state = 0
  
  def receive = {
    case GetData() => sender() ! CarData(Some(name), latitude, longitude)
    case CarData(n, lat, long) => {
      n match { case  Some(nn) => name = nn case None => {} }
      latitude = lat
      longitude = long
    }
  }
}