import java.sql.Timestamp

import model._
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.Future

class UserTable(tag: Tag) extends Table[User](tag, "users") {
  val id = column[Int]("id", O.PrimaryKey)
  val email = column[String]("email")
  val first_name = column[String]("first_name")
  val last_name = column[String]("last_name")
  val gender = column[Char]("gender")
  val birth_date = column[Timestamp]("birth_date")

  def * =
    (id, email, first_name, last_name, gender, birth_date) <> (User.apply _ tupled, User.unapply)
}

object UserTable {
  val table = TableQuery[UserTable]
}

class UserRepo(db:Database) {
  val table = TableQuery[UserTable]

  def create(user: User) = db.run(table returning table += user)

  def update(user: User) = db.run(table.filter(_.id === user.id).update(user))

  def getById(id: Int) = db.run(table.filter(_.id === id).result.headOption)

  def updateUserFirstName(id: Int, first_name: String) =
    db.run(table.filter(_.id === id).map(_.first_name).update(first_name))

  def updateUserLastName(id: Int, last_name: String) =
    db.run(table.filter(_.id === id).map(_.last_name).update(last_name))

  def updateUserEmail(id: Int, email: String) =
    db.run(table.filter(_.id === id).map(_.first_name).update(email))

  def updateUserGender(id: Int, gender: Char) =
    db.run(table.filter(_.id === id).map(_.gender).update(gender))

  def updateUserBirthDate(id: Int, birth_date: Timestamp) =
    db.run(table.filter(_.id === id).map(_.birth_date).update(birth_date))

}


