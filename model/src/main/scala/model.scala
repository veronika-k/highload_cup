import java.sql.Timestamp

import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.JsonCodec

package object model {
  @JsonCodec
  case class User(id: Int,
                  email: String,
                  first_name: String,
                  last_name: String,
                  gender: Char,
                  birth_date: Timestamp)


  @JsonCodec
  case class Location(distance: Int,
                      city: String,
                      place: String,
                      id: Int,
                      country: String)

  @JsonCodec
  case class Visit(id: Int,
                   location: Int,
                   user: Int,
                   visited_at: Timestamp,
                   mark: Int)


  @JsonCodec
  case class VisitQueryItem(mark:Int, visited_at: Timestamp, place: String)

  @JsonCodec
  case class VisitsQueryList(visits:List[VisitQueryItem])

  @JsonCodec
  case class Avg(avg:String)


  implicit val TimestampFormat : Encoder[Timestamp] with Decoder[Timestamp] = new Encoder[Timestamp] with Decoder[Timestamp] {
    override def apply(a: Timestamp): Json = Encoder.encodeLong.apply(a.getTime)

    override def apply(c: HCursor): Result[Timestamp] = Decoder.decodeLong.map(s => new Timestamp(s)).apply(c)
  }



}

