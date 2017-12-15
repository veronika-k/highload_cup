/**
  * Created by inoquea on 11.12.17.
  */
import java.io.File

import java.sql.Timestamp

import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import io.circe.parser.decode

import io.circe.Decoder.Result
import io.circe._
import io.circe.parser._
import model.{User, _}

import scala.io.Source

trait Repos extends Unzip{

  val db = Database.forURL(
//    "jdbc:postgresql://localhost:5432/airport?user=inoquea&password=11111111"
      "jdbc:postgresql://ec2-174-129-37-15.compute-1.amazonaws.com:5432/dbio76lfn642mk?sslmode=require&user=ihlufrlpffkzps&password=b7541238210dd2e41867d591b503a1ca4fbac843eec43897101e622583d8b1f6")
  val userRepo = new UserRepo(db)
  val locationRepo = new LocationRepo(db)
  val visitRepo = new VisitRepo(db)
  
  def init(): Unit = {
    Await.result(db.run(userRepo.table.schema.create), Duration.Inf)
    Await.result(db.run(locationRepo.table.schema.create), Duration.Inf)
    Await.result(db.run(visitRepo.table.schema.create), Duration.Inf)
  }
  
  def getDataFromJson(file:File, downField:String) ={
    val str = Source.fromFile(file).getLines.toList.head
    val json = parse(str).getOrElse(Json.Null)
    json.hcursor
      .downField(downField)
      .focus
      .flatMap(_.asArray)
      .getOrElse(Vector.empty)
  }
  
  def full() = {
    unZipIt(INPUT_ZIP_FILE, OUTPUT_FOLDER)
    val d = new File(OUTPUT_FOLDER)
    for (fname<-d.listFiles.toList) fullBy(fname)
  }

  def fullBy(file:File) = {
    file.getName.split("_")(0) match{
      case "users" => {
        val users = getDataFromJson(file,"users").map(_.as[User].getOrElse(Nil))
        for (user<-users)
          userRepo.create(user.asInstanceOf[User])

      }
      case "visits" => {
        val visits = getDataFromJson(file,"visits").map(_.as[Visit].getOrElse(Nil))
        for (v<-visits)
          visitRepo.create(v.asInstanceOf[Visit])

      }
      case "locations" => {
        val locations = getDataFromJson(file,"locations").map(_.as[Location].getOrElse(Nil))
        for (l<-locations)
          locationRepo.create(l.asInstanceOf[Location])

      }
      case _ => print()
    }
  }

  def distanceQuery(distance:Int) ={
    locationRepo.table
      .filter(_.distance < distance)
  }

  def countryQuery(country:String) ={
    locationRepo.table
      .filter(_.country === country)
  }

  def distanceCountryQuery(distance:Int, country:String) ={
    locationRepo.table
      .filter{case location => location.country === country && location.distance < distance}
  }
  def datesQuery(id:Int, fromDate:Timestamp, toDate:Timestamp) ={
    visitRepo.table
      .filter{case visit => visit.visited_at >fromDate && visit.visited_at < toDate && visit.user === id}
  }

  def getAllVisitsWithParms(id:Int, distance:Int, country:String, fromDate:Timestamp, toDate:Timestamp ) = {
    val zeroTime = new Timestamp (0)
    val userVisitQuery = {if (fromDate!=zeroTime && toDate!=zeroTime){
      userRepo.table.filter(_.id === id)
        .join(datesQuery(id,fromDate,toDate)).on(_.id ===_.user)
    }
    else {
      userRepo.table.filter(_.id === id)
        .join(visitRepo.table).on(_.id === _.user)

      }
    }
    val query = {(distance, country) match {
      case(0, "0") => {
        userVisitQuery
          .join(locationRepo.table)
          .on{case((user,visit),location) => visit.location === location.id}
      }
      case (0, country) => userVisitQuery
          .join(countryQuery(country))
          .on{case((user,visit),location) => visit.location === location.id}
      case (distance, "0") => userVisitQuery
        .join(distanceQuery(distance))
        .on{case((user,visit),location) => visit.location === location.id}

      case (distance, country) => userVisitQuery
        .join(distanceCountryQuery(distance, country))
        .on{case((user,visit),location) => visit.location === location.id}
      }
    }
    val finalQuery = query.map{case((user,visit),location) => (visit.mark, visit.visited_at, location.place)}
    db.run(finalQuery.result)
  }

  def datesLocationQuery(fromDate:Timestamp, toDate:Timestamp) ={
    visitRepo.table
      .filter{case visit => visit.visited_at >fromDate && visit.visited_at < toDate }
  }


  def ageFromQuery(fromAge: Int) = {
    val millisInYear = 31536000000L
    val time = System.currentTimeMillis() - fromAge * millisInYear
    val fromYear = new Timestamp(time)
    userRepo.table.filter{case user => user.birth_date < fromYear}
  }

  def ageToQuery(toAge: Int) = {
    val millisInYear = 31536000000L
    val time = System.currentTimeMillis() - toAge * millisInYear
    val toYear = new Timestamp(time)
    userRepo.table.filter{case user => user.birth_date > toYear}
  }

  def ageFromTo(fromAge: Int,toAge: Int) ={
    val millisInYear = 31536000000L
    val fromYear = new Timestamp(System.currentTimeMillis() - fromAge * millisInYear)
    val toYear = new Timestamp(System.currentTimeMillis() - toAge * millisInYear)
    userRepo.table.filter{case user => user.birth_date < fromYear && user.birth_date > toYear}
  }

  def getLocationsWithParms(id:Int, fromDate:Timestamp, toDate:Timestamp, fromAge: Int,toAge: Int, gender:Char ) = {
    val zeroTime = new Timestamp (0)
    val userQuery = {
      (fromAge, toAge, gender) match {
        case (0,0,'0')  => userRepo.table
        case (0,0,gender) => userRepo.table.filter{case user => user.gender === gender}
        case (fromAge,0,'0') => ageFromQuery(fromAge)
        case (fromAge,0,gender) => ageFromQuery(fromAge).filter{case user => user.gender === gender}
        case (0,toAge,'0') => ageToQuery(toAge)
        case (0,toAge,gender) => ageToQuery(toAge).filter{case user => user.gender === gender}
        case (fromAge,toAge,'0') => ageFromTo(fromAge,toAge)
        case (fromAge,toAge,gender) => ageFromTo(fromAge,toAge).filter{case user => user.gender === gender}
      }
    }

    val visitQuery = if (fromDate!=zeroTime && toDate!=zeroTime){
      datesLocationQuery(fromDate,toDate)
    }
    else visitRepo.table

    val locationQuery = locationRepo.table.filter(_.id === id)
    val query = visitQuery.join(locationQuery).on{case (visit,location) => visit.location === location.id}
      .join(userQuery).on{case((visit,location), user) => visit.user === user.id}
      .map{case((visit,location), user) => visit.mark}
      .map(_.asColumnOf[Double])
      .avg
    db.run(query.result)
  }


  def visitQuery(id:Int) ={
    userRepo.table.filter(_.id === id)
      .join(visitRepo.table).on(_.id ===_.user)
      .join(locationRepo.table)
      .on{case((user,visit),location) => visit.location === location.id}
  }

  def getA(id:Int) = {
    val query = visitQuery(id)
      .map{case((user,visit),location) => (user.id, location.country, visit.visited_at)}
    db.run(query.result)
  }

  def getFlagTime(str: Option[String]) = str match {
    case None => true
    case Some(_) => {
    {
      decode[Timestamp](str.getOrElse("fail").split(" HTTP")(0)) match {
        case Right(_) => true
        case Left(_) => false
      }
    }
  }
  }

  def getFlagInt(str: Option[String]) = str match {
    case None => true
    case Some(_) => {
      {
        decode[Int](str.getOrElse("fail").split(" HTTP")(0)) match {
          case Right(_) => true
          case Left(_) => false
        }
      }
    }
  }

  def getFlagChar(str: Option[String]) = str match {
    case None => true
    case Some("") => false
    case Some(_) => true
  }

  def decodeTime(str: Option[String]) ={
    decode[Timestamp](str.getOrElse("0").split(" HTTP")(0)) match {
      case Right(time) => time
    }
  }

  def decodeInt(str: Option[String]): Int ={
    decode[Int](str.getOrElse("0").split(" HTTP")(0)) match {
      case Right(num) => num
    }
  }

  def decodeChar(str: Option[String]): Char ={
    str.getOrElse("0").split(" HTTP")(0)(0)
  }

  def getFlagString(str: Result[String]): Boolean =
  {
    str match {
      case Left(ex) => ex.message != "String"
      case Right(_) => true
    }
  }
}
