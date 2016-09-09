package controllers

import akka.actor.{ActorRef, ActorSelection, ActorSystem}
import javax.inject._

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import actors.CarActor._
import play.api._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import actors._
import play.api.libs.json._
import akka.util.Timeout
import akka.pattern.ask
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.duration._
import akka.pattern.ask

class CarController @Inject() (actorSystem: ActorSystem) extends Controller {
  implicit val timeout = akka.util.Timeout(5.seconds)

  def createCar (name: String) = Action(BodyParsers.parse.json) {
    val car = actorSystem.actorOf(CarActor.props, name)
    Created(car.path.toString())
  }

  def getCar (name: String) = Action.async {
    val car = actorSystem.actorSelection(getCarUri(name))
    getCarResource(car)
  }

  implicit val locationReads: Reads[Position] = ((JsPath \ "latitude").read[Double] and (JsPath \ "longitude").read[Double])(Position.apply _)

  def updateCar (name: String) = Action(BodyParsers.parse.json) { request =>
    val newPosition = request.body.validate[Position]
    newPosition.fold(
      errors => { BadRequest("error!")},
      position => {
        val car = actorSystem.actorSelection(getCarUri(name))
        car ! position
        //getCarResource(car)
        Ok("")
      }
    )
  }

  private def getCarUri(name : String): String = s"akka://application/user/$name"

  private def getCarResource (car: ActorSelection): Future[Result] ={
    (car ? GetPosition()).mapTo[Position].map { position =>
      Ok(Json.obj(
        "lat" -> position.latitude,
        "long" -> position.longitude
      ))
    }
  }
}
