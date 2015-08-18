import dispatch._
import org.scalatest.FunSuite

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
/**
 * Created by alespuh on 13.05.15.
 */
//class FsmSyncPerformance_Test extends FunSuite
//{
//    val guids = List("c80d163289014b61a069f27160569a61",
//        "000065a742b74aff8646df94ffa91c98",
//        "0000cc18e9cb4fafb168158077c59f10",
//        "0001b84ad75047feb88a0ab5c16ee37c",
//        "000409c1c65942c587ff0fad8efb7aaa",
//        "000660c08d444d38a737fc572e66def4",
//        "000a08359fe940ebb11446bf05b0b11a",
//        "000c8a4c1dbd472f910e5ede26c7c553",
//        "000caa51dd2c43bb882ebd8691ac0983",
//        "001078bd5a0f4ff2a46e371fd67a592a",
//        "001125b0bf614752b422159ac9b29fd5",
//        "0011f21e8cdf40b8a3ef8b9f6fcfa8b0",
//        "0016a2765d454cc0b122fbda8e519791",
//        "001ae85f7e894280bb7fb00801bbe04e",
//        "002529e1cd304ebeb155870bf1312121",
//        "0026213e091c47649197fc4b21efce73",
//        "00268c02f8aa4276a7c85870e454fd48",
//        "002a8acb88f34edbbbd6eb7cb3f27a51",
//        "003031ad5ce244ca9e1758ef6de9f61a",
//        "0031e3f898a842ae9d58157bd69aa451",
//        "00353e70b1bd4621a0cb0042df1eb807")
//
//    def sendGet(guid: String) =
//        Http(s"http://localhost:9000/candidates/$guid?backActionCode=list").execute()
//
//    test("run")
//    {
//        val futures = for(i <- 1 to 120) yield Future
//        {
//            while(true)
//            {
//                guids.map(sendGet)
//            }
//        }
//        Await.result(FutureHelper.all(futures.toList), 10 minute)
//    }
//}

class FsmSyncPerformance_Test extends FunSuite
{
    val urls = /*List("c80d163289014b61a069f27160569a61",
        "000065a742b74aff8646df94ffa91c98",
        "0000cc18e9cb4fafb168158077c59f10",
        "0001b84ad75047feb88a0ab5c16ee37c",
        "000409c1c65942c587ff0fad8efb7aaa",
        "000660c08d444d38a737fc572e66def4",
        "000a08359fe940ebb11446bf05b0b11a",
        "000c8a4c1dbd472f910e5ede26c7c553",
        "000caa51dd2c43bb882ebd8691ac0983",
        "001078bd5a0f4ff2a46e371fd67a592a",
        "001125b0bf614752b422159ac9b29fd5",
        "0011f21e8cdf40b8a3ef8b9f6fcfa8b0",
        "0016a2765d454cc0b122fbda8e519791",
        "001ae85f7e894280bb7fb00801bbe04e",
        "002529e1cd304ebeb155870bf1312121",
        "0026213e091c47649197fc4b21efce73",
        "00268c02f8aa4276a7c85870e454fd48",
        "002a8acb88f34edbbbd6eb7cb3f27a51",
        "003031ad5ce244ca9e1758ef6de9f61a",
        "0031e3f898a842ae9d58157bd69aa451",
        "00353e70b1bd4621a0cb0042df1eb807").map(getCandidateUrl) ++ */List(
            "a016fbeb4979421cb9b1bd49c3660209",
            "85adbd6940274de1ae1185201bfb8500",
            "b39e88824fb34724b4dee83f3bbd028e",
            "b511366347e846f995aa477db54e64c7",
            "8fa170f86c2e4b369bf4d855770cae42",
            "90a0a7cac3bf479aac7a9bd94f413f07",
            "09adaf7e81ec47148fa07bd54e325cc1"
    ).map(getOrgToCycleUrl)

    val count = urls.size

    def getOrgToCycleUrl(orgToCycleId: String) = s"http://localhost:9000/orgToCycle/$orgToCycleId"

    def getCandidateUrl(guid: String) = s"http://localhost:9000/candidates/$guid?backActionCode=list"

    def sendRequests(index: Int): Future[String] =
    {
        def sendOne(cardUrl: String): Future[String] =
        {
            val svc = url(cardUrl).setHeader("Cookie", "PLAY_SESSION=\"5a4ebc8a10772e84be0d96b6c39da8a61beae097-sessionId=oprxcgmg2ure.o4i%28pqv4agb5t3f-3zn2nlrx%29sltwd%29.k1vsz.4%7E.8tx%217gg5m%21\"; REMEMBER_ME=\"ecd33a8d3de62b20fd62feef314f916783012da1-userId=10482&series=6841998391575880615&token=922261720327718336\"; org-list-selected-admission-cycle=ced826c45a2c48a09112baf2016eaedb; candidate-list-selected-admission-cycle=29c450fbcca5475c8373e5861b6fdd79; CandidateBackAction=list")
            Http(svc OK as.String).flatMap(x => sendOne(cardUrl))
        }
        sendOne(urls(index % count))
    }

    test("run")
    {
        val futures = for(i <- 1 to 300) yield sendRequests(i)
        Await.result(Future.sequence(futures), 10 minute)
    }
}