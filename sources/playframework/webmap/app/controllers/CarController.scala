package controllers

import javax.inject._

import actors.CarActor._
import actors._
import akka.actor.{ActorSelection, ActorSystem}
import akka.pattern.ask
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.Future
import scala.concurrent.duration._

class CarController @Inject() (actorSystem: ActorSystem) extends Controller {
  implicit val timeout = akka.util.Timeout(5.seconds)
  implicit val locationReads: Reads[CarData] = (
    (JsPath \ "name").readNullable[String] and
    (JsPath \ "latitude").read[Double] and
    (JsPath \ "longitude").read[Double]
    )(CarData.apply _)

  def createCar = Action(BodyParsers.parse.json) { request =>
    val carData = request.body.validate[CarData]
    carData.fold(
      errors => { BadRequest("error!") },
      newCar => {
        val car = actorSystem.actorOf(CarActor.props, newCar.name.get)
        car ! newCar
        Ok("")
      })
  }

  def getCar (name: String) = Action.async {
    val car = actorSystem.actorSelection(getCarUri(name))
    returnCarResource(car)
  }

  def updateCar (name: String) = Action(BodyParsers.parse.json) { request =>
    val carData = request.body.validate[CarData]
    carData.fold(
      errors => { BadRequest("error!")},
      newCar => {
        val car = actorSystem.actorSelection(getCarUri(name))
        car ! newCar
        //returnCarResource(car)
        Ok("")
      }
    )
  }

  private def getCarUri(name : String): String = s"akka://application/user/$name"

  private def returnCarResource (car: ActorSelection): Future[Result] ={
    (car ? GetData()).mapTo[CarData].map { car =>
      Ok(Json.obj(
        "name" -> car.name,
        "lat" -> car.latitude,
        "long" -> car.longitude
      ))
    }
  }
}
