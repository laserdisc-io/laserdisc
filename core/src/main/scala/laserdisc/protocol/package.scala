package laserdisc

package object protocol {
  private[protocol] final val KVPairRegex = "(.*):(.*)".r

  private[protocol] val KVP: String ==> (String, String) = {
    case KVPairRegex(k, v) => Right(k -> v)
    case other             => Left(RESPDecErr(s"String ==> (String, String), error decoding key -> value pair. Expected [key:value] but was $other"))
  }

  private[protocol] val KVPS: Seq[String] ==> List[(String, String)] = Read.instance { ss =>
    ss.foldRight[RESPDecErr | (List[(String, String)], Int)](Right(Nil -> 0)) {
        case (KVP(Right((k, v))), Right((kvs, kvl))) => Right(((k -> v) :: kvs) -> (kvl + 1))
        case (KVP(Left(e)), Right((_, kvl))) =>
          Left(RESPDecErr(s"List[String] ==> List[(String, String)], Error decoding key value pairs at position ${kvl + 1}. Error was: $e"))
        case (_, left) => left
      }
      .map(_._1)
  }
}
