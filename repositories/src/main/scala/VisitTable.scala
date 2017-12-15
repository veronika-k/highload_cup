import java.sql.Timestamp

import model._
import slick.lifted.Tag
import slick.jdbc.PostgresProfile.api._

class VisitTable (tag: Tag) extends Table[Visit](tag, "visit") {
  val id = column[Int]("id", O.PrimaryKey)
  val location = column[Int]("location")
  val user = column[Int]("user")
  val visited_at = column[Timestamp]("visited_at")
  val mark = column[Int]("mark")
  val location_fk = foreignKey("location_fk", location, TableQuery[LocationTable])(_.id)
  val user_fk = foreignKey("user_fk", user, TableQuery[UserTable])(_.id)

  def * =
    (id, location, user, visited_at, mark) <> (Visit.apply _ tupled, Visit.unapply)

}

object VisitTable {
  val table = TableQuery[VisitTable]
}

class VisitRepo(db:Database){
  val table = TableQuery[VisitTable]
  def create(visit: Visit) = db.run(table returning table += visit)
  def update(visit:Visit) = db.run(table.filter(_.id === visit.id).update(visit))
  def getById(id:Int) = db.run(table.filter(_.id === id).result.headOption)
}