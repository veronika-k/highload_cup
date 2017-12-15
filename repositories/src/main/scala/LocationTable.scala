import model._
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

class LocationTable(tag: Tag) extends Table[Location](tag, "location") {
  val id = column[Int]("id", O.PrimaryKey)
  val place = column[String]("place")
  val country = column[String]("country")
  val city = column[String]("city")
  val distance = column[Int]("distance")

  def * =
    ( distance, city,  place, id, country) <> (Location.apply _ tupled, Location.unapply)
}

object LocationTable {
  val table = TableQuery[LocationTable]
}

class LocationRepo(db:Database){
  val table = TableQuery[LocationTable]
  def create(location: Location) = db.run(table returning table += location)
  def update(location:Location) = db.run(table.filter(_.id === location.id).update(location))
  def getById(id:Int) = db.run(table.filter(_.id === id).result.headOption)
}