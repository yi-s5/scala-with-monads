package with_monads

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.exceptions.NotAllowedException

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

    "Reader" should "Perform Login" in {

        def getUser(username: String): Reader[Environment, Option[User]] = Reader(
            env => env.usersByUsername.get(username)
        )

        def getUserPassword(username: String): Reader[Environment, Option[String]] =
            for {
                userOpt     <- getUser(username)
                passwordOpt = userOpt.fold[Option[String]](None)(user => Some(user.password))
            } yield (passwordOpt)

        def validateUsernameAndPassword(username: String, password: String): Reader[Environment, Boolean] = 
            for {
                passwordOpt <- getUserPassword(username)
            } yield(
                passwordOpt.fold(false)(correctPass => correctPass.equals(password))
                )

        def getListUserAuth(user: User): Reader[Environment, Option[List[UserAuth]]] = Reader(
            env => env.userAuthByUser.get(user)
        )

        def auth(username: String, password: String): Reader[Environment, Option[List[UserAuth]]] = {
            // val authorized = for {
            //     bool <- validateUsernameAndPassword(username, password)
            // } yield(bool)

            val getOptList = for {
                    
                    userOpt     <- getUser(username)
                    listAuthOpt <- getListUserAuth(userOpt.get)
                } yield(listAuthOpt)
            getOptList

        }

    assert(false)        

    }
}