package com.lkroll.ep.chargen.names

import org.apache.pekko.actor._
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.stream.{ActorMaterializer, ActorMaterializerSettings}
import org.apache.pekko.util._

import upickle.default.{ReadWriter => RW, _}

import scala.language.postfixOps
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scala.collection.mutable

import com.lkroll.ep.chargen.character.Skill
import com.lkroll.ep.compendium.{GenderIdentity, Language}
import com.lkroll.ep.compendium.data.Languages

import com.typesafe.scalalogging.StrictLogging
import com.typesafe.config.Config

trait BehindTheNameAPI {
  def randomName(): Future[String];
  def randomName(gender: GenderIdentity, nativeLanguage: Skill, num: Int = 1): Future[String];
}

object BehindTheName extends BehindTheNameAPI {
  private lazy val _instance: BehindTheName = {
    val system = ActorSystem("BehindTheName");
    new BehindTheName(system)
  };

  override def randomName(): Future[String] = _instance.randomName();
  override def randomName(gender: GenderIdentity, nativeLanguage: Skill, num: Int = 1): Future[String] =
    _instance.randomName(gender, nativeLanguage, num);

  val source = "https://www.behindthename.com";

  def genTimeout(config: Config = _instance.system.settings.config): FiniteDuration = {
    Duration.fromNanos(config.getDuration("chargen.gen-timeout", java.util.concurrent.TimeUnit.NANOSECONDS));
  }
}

class BehindTheName(val system: ActorSystem) extends BehindTheNameAPI with StrictLogging {
  import org.apache.pekko.pattern.ask;
  import system.dispatcher;

  private val apiKeyT = Try {
    val key = system.settings.config.getString("chargen.behindthename.api-key");
    require(key != "<PUT YOUR API KEY HERE>", "You must provide a valid API Key for Behind the Name!");
    key
  };

  val genTimeout = BehindTheName.genTimeout(system.settings.config)

  private val clientT =
    apiKeyT.map(apiKey => system.actorOf(Props(new BehindTheNameClient(apiKey)), name = "behind-the-name-client"));

  private val cancellableT = clientT.map { client =>
    system.scheduler.scheduleAtFixedRate(250 milliseconds, 250 milliseconds, client, Tick)
  };

  implicit val timeout: Timeout = Timeout(5 seconds);

  override def randomName(): Future[String] = {
    val req = new RandomNameRequest(num = 1, randomSurname = true);
    executeRequest(req)
  }

  override def randomName(gender: GenderIdentity, nativeLanguage: Skill, num: Int = 1): Future[String] = {
    import GenderIdentity._;
    val gcode = gender match {
      case Male   => Some(GenderCode.m)
      case Female => Some(GenderCode.f)
      case _      => None
    };
    val lang = nativeLanguage.field.flatMap(Languages.from(_));
    val usage = lang.map(LanguageUsage.lookup(_)).getOrElse(Nil);
    logger.debug(s"Usage for native language '${lang}': $usage");

    val req = new RandomNameRequest(gender = gcode, usage = usage, num = num, randomSurname = true);
    executeRequest(req)
  }

  private def executeRequest(req: NameRequest): Future[String] = {
    clientT match {
      case Success(client) => {
        val f = (client ? req).mapTo[NameResult];
        f.map(_.names.mkString(" "))
      }
      case Failure(f) => Future.failed(f)
    }
  }
}

case class NameResult(names: List[String])
object NameResult {
  implicit def rw: RW[NameResult] = macroRW;
}

sealed trait NameRequest {
  def generateUri(apiKey: String): Uri;
}
case class RandomNameRequest(gender: Option[GenderCode.GenderCode] = None,
                             usage: List[NameUsageCode.NameUsageCode] = Nil,
                             num: Int = 2,
                             randomSurname: Boolean = false)
    extends NameRequest {
  require(num > 0);
  require(num <= 6);

  override def generateUri(apiKey: String): Uri = {
    val opts = List(
      Some("key" -> apiKey),
      gender.map(c => ("gender" -> GenderCode.toQuery(c))),
      Some("number" -> num.toString),
      Some("randomsurname" -> (if (randomSurname) "yes" else "no"))
    ).flatten ++
      usage.map(c => ("usage" -> NameUsageCode.toQuery(c)));

    Uri("https://www.behindthename.com/api/random.json").withQuery(Uri.Query(opts: _*))
  }
}
case object Tick

private[names] case class OutstandingRequest(request: NameRequest, replyTo: ActorRef)

class BehindTheNameClient(apiKey: String) extends Actor with ActorLogging {
  import org.apache.pekko.pattern.pipe;
  import context.dispatcher;

  implicit val system: ActorSystem = context.system

  val http = Http(context.system);

  private val requests = mutable.Queue.empty[OutstandingRequest];

  override def receive = {
    case req: RandomNameRequest => {
      requests += OutstandingRequest(req, sender());
    }
    case Tick => {
      if (!requests.isEmpty) {
        val req = requests.dequeue();
        val uri = req.request.generateUri(apiKey);
        val request = HttpRequest(uri = uri);
        log.debug(s"Generated request:\n$request")
        http.singleRequest(request).flatMap(parseJson).pipeTo(req.replyTo)
      }
    }
  }

  private def parseJson(response: HttpResponse): Future[NameResult] = {
    response match {
      case HttpResponse(StatusCodes.OK, _, entity, _) => {
        val res = entity.dataBytes.runFold(ByteString(""))(_ ++ _);
        res.flatMap { body =>
          val content = body.utf8String;
          val res: Try[NameResult] = Try {
            log.debug("Got response, body: " + content);
            read[NameResult](content)
          };
          val resF: Future[NameResult] = res match {
            case Success(nr) => Future.successful(nr)
            case Failure(t) => {
              log.error(t, s"Could not deserialise message:\n${content}");
              Future.failed(t)
            }
          }
          resF
        }
      }
      case resp @ HttpResponse(code, _, _, _) => {
        log.warning(s"Request failed, response code: $code")
        resp.discardEntityBytes()
        Future.failed(new RuntimeException(s"Invalid Response code: $code"))
      }
    }
  }
}

object GenderCode extends Enumeration {
  type GenderCode = Value;

  val f, m = Value;

  def toQuery(code: GenderCode): String = code.toString();
}
