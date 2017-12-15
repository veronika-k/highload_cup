import java.sql.Timestamp

import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import model._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait ApiRoutes extends Repos with FailFastCirceSupport {
  val route = {
    pathPrefix(Segment) { str: String =>
      pathPrefix(IntNumber) { id: Int =>
        {
          pathEndOrSingleSlash {
            get {
              if (str == "users") {
                onComplete(userRepo.getById(id)) {
                  case util.Success(Some(result)) => complete(result.asJson)
                  case util.Success(None)         => complete(NotFound)
                  case util.Failure(ex)           => complete(BadRequest)
                }
              } else {
                if (str == "locations") {
                  onComplete(locationRepo.getById(id)) {
                    case util.Success(Some(result)) => complete(result.asJson)
                    case util.Success(None)         => complete(NotFound)
                    case util.Failure(ex)           => complete(BadRequest)
                  }
                } else {
                  if (str == "visits") {
                    onComplete(visitRepo.getById(id)) {
                      case util.Success(Some(result)) => complete(result.asJson)
                      case util.Success(None)         => complete(NotFound)
                      case util.Failure(ex)           => complete(BadRequest)
                    }
                  } else complete(BadRequest)
                }
              }
            }
          } ~
            pathPrefix("visits") {
              parameters('distance.?, 'fromDate.?, 'toDate.?, 'country.?) {
                (distance, fromDate, toDate, country) =>
                  get {
                    if (str == "users" && getFlagTime(fromDate) && getFlagTime(
                          toDate) && getFlagInt(distance)) {
                      onComplete(
                        getAllVisitsWithParms(id,
                                              decodeInt(distance),
                                              country.getOrElse("0"),
                                              decodeTime(fromDate),
                                              decodeTime(toDate))) {
                        case util.Success(result) =>
                          if (result != Nil) {
                            val json = result.flatMap {
                              case (mark, visited_at, plase) =>
                                List(
                                  new VisitQueryItem(mark, visited_at, plase))
                            }.toList
                            complete(new VisitsQueryList(json).asJson)
                          } else complete(NotFound)
                        case util.Failure(ex) => complete(BadRequest)
                      }
                    } else
                      complete(BadRequest)
                  }
              }
            } ~
            pathPrefix("avg") {
              parameters('fromAge.?,
                         'toAge.?,
                         'fromDate.?,
                         'toDate.?,
                         'gender.?) {
                (fromAge, toAge, fromDate, toDate, gender) =>
                  get {
                    if (str == "locations" && getFlagTime(fromDate) && getFlagTime(
                          toDate) && getFlagInt(fromAge) && getFlagInt(toAge) && getFlagChar(
                          gender)) {
                      onComplete(
                        getLocationsWithParms(id,
                                              decodeTime(fromDate),
                                              decodeTime(toDate),
                                              decodeInt(fromAge),
                                              decodeInt(toAge),
                                              decodeChar(gender))) {
                        case util.Failure(ex) => complete(BadRequest)
                        case util.Success(Some(result)) =>
                          complete(new Avg(f"$result%1.5f").asJson)
                        case util.Success(None) => complete(NotFound)
                      }
                    } else
                      complete(BadRequest)
                  }
              }
            } ~ {
            str match {
              case "users" =>
                post {
                  entity(as[Json]) { json =>
                    val email = json.hcursor.downField("email").as[String]
                    val first_name =
                      json.hcursor.downField("first_name").as[String]
                    val last_name =
                      json.hcursor.downField("last_name").as[String]
                    val gender = json.hcursor.downField("gender").as[Char]
                    val birth_date =
                      json.hcursor.downField("birth_date").as[Timestamp]
                    if(getFlagString(email) && getFlagString(first_name) && getFlagString(last_name)){
                    onComplete {
                      val userForUp =
                        Await.result(userRepo.getById(id), Duration.Inf).head
                      val user = userForUp match {
                        case oldUser: User =>
                          new User(
                            id,
                            email.getOrElse(oldUser.email),
                            first_name.getOrElse(oldUser.first_name),
                            last_name.getOrElse(oldUser.last_name),
                            gender.getOrElse(oldUser.gender),
                            birth_date.getOrElse(oldUser.birth_date)
                          )
                      }
                      userRepo.update(user)
                    } {
                      case util.Success(value) => complete("User updated")
                      case util.Failure(ex)    => complete(NotFound)
                    }
                    } else complete(BadRequest)
                  }
                }

              case "locations" =>
                post {
                  entity(as[Json]) { json =>
                    val distance = json.hcursor.downField("distance").as[Int]
                    val city = json.hcursor.downField("city").as[String]
                    val place = json.hcursor.downField("place").as[String]
                    val country = json.hcursor.downField("country").as[String]
                    if (getFlagString(country) && getFlagString(city) && getFlagString(
                          place)) {
                      onComplete {
                        val locationForUp = Await
                          .result(locationRepo.getById(id), Duration.Inf)
                          .head
                        val location = locationForUp match {
                          case oldLocation: Location =>
                            new Location(
                              distance.getOrElse(oldLocation.distance),
                              city.getOrElse(oldLocation.city),
                              place.getOrElse(oldLocation.place),
                              id,
                              country.getOrElse(oldLocation.country)
                            )
                        }
                        locationRepo.update(location)
                      } {
                        case util.Success(value) => complete("Location updated")
                        case util.Failure(ex)    => complete(NotFound)
                      }
                    } else complete(BadRequest)
                  }
                }
              case "visits" =>
                post {
                  entity(as[Json]) { json =>
                    val location = json.hcursor.downField("location").as[Int]
                    val user = json.hcursor.downField("user").as[Int]
                    val visited_at =
                      json.hcursor.downField("visited_at").as[Timestamp]
                    val mark = json.hcursor.downField("mark").as[Int]
                    onComplete {
                      val visitForUp =
                        Await.result(visitRepo.getById(id), Duration.Inf).head
                      val visit = visitForUp match {
                        case oldVisit: Visit =>
                          new Visit(id,
                                    location.getOrElse(oldVisit.location),
                                    user.getOrElse(oldVisit.user),
                                    visited_at.getOrElse(oldVisit.visited_at),
                                    mark.getOrElse(oldVisit.mark))
                      }
                      visitRepo.update(visit)
                    } {
                      case util.Success(value) => complete("Visit updated")
                      case util.Failure(ex)    => complete(NotFound)
                    }
                  }
                }
              case _ => {
                post {
                  complete(BadRequest)
                }
              }
            }
          }
        }

      } ~
        pathPrefix("new") {
          str match {
            case "users" =>
              post {
                entity(as[User]) { user =>
                  onComplete(userRepo.create(user)) {
                    case util.Success(value) => complete("User created")
                    case util.Failure(ex)    => complete(BadRequest)
                  }
                }
              }

            case "locations" =>
              post {
                entity(as[Location]) { location =>
                  onComplete(locationRepo.create(location)) {
                    case util.Success(value) => complete("Location created")
                    case util.Failure(ex)    => complete(BadRequest)
                  }
                }
              }
            case "visits" =>
              post {
                entity(as[Visit]) { visit =>
                  onComplete(visitRepo.create(visit)) {
                    case util.Success(value) => complete("Visit created")
                    case util.Failure(ex)    => complete(BadRequest)
                  }
                }
              }
            case _ => {
              post {
                complete(BadRequest)
              }
            }
          }
        }
    }
  }

}
