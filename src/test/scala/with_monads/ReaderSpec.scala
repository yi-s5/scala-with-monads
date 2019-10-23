package with_monads


import org.scalatest._
import org.scalatest.Matchers._

class ReaderSpec extends FlatSpec {
  
  sealed trait UserAuth
  final object ReadAccess extends UserAuth
  final object WriteAccess extends UserAuth
  final object AdminAccess extends UserAuth

  case class User(username: String, password: String)

  case class Environment(
    usersByUsername: Map[String, User],
    userAuthByUser: Map[User, List[UserAuth]]
  )

  val alex = User("alex.mills", "abc123")
  val yi = User("yi.fu", "b35t p455w0rd Ev4!")
  val corey = User("corey.mckenzie", "0m6 s0 57roN6!")
  val thomas = User("thomas.ibarra", "N3v34 6o1n6 7o 6u3ss 17!")

  val env = new Environment(
    Map("alex.mills" -> alex,
        "yi.fu" -> yi,
        "corey.mckenzie" -> corey,
        "thomas.ibarra" -> thomas),
    Map(alex   -> List(ReadAccess),
        yi     -> List(WriteAccess, ReadAccess),
        thomas -> List(AdminAccess, WriteAccess, ReadAccess),
        corey  -> List(AdminAccess, WriteAccess))
  )

  "Reader" should "interact with password login" in {
    def getUser(username: String): Reader[Environment, Option[User]] = 
      Reader(env => env.usersByUsername.get(username))
    
    def getUserPassword(username: String): Reader[Environment, Option[String]] =
      Reader(env => env.usersByUsername.get(username).map(_.password))

    def validateUsernameAndPassword(username: String, password: String): Reader[Environment, Boolean] = 
      for {
        userOpt <- getUser(username)
        pwdOpt  <- getUserPassword(username)
      } yield (userOpt.flatMap(_ => pwdOpt)
                      .map(_ == password)
                      .getOrElse(false))
    
    def getUserAuth(user: Option[User]): Reader[Environment, Option[List[UserAuth]]] = 
      Reader(env => user.flatMap(env.userAuthByUser.get(_)))

    def retrieveUserAuthForValidUser(username: String, password: String): Reader[Environment, Option[List[UserAuth]]] =
      for {
        isValid <- validateUsernameAndPassword(username, password)
        userOpt <- getUser(username)
        userAuthOpt <- getUserAuth(userOpt)
      } yield (userAuthOpt)
  }
}