package com.github.petekneller

object urls {

  private val schemePart = """(\w+)://([\w/?&#=.]+)""".r
  private val authorityPart = """([\w.]+)(/[\w/?&#=.]+)""".r
  private val authorityPartNoRest = """([\w.]+)""".r
  private val hierarchicalPart = """([\w/.]+)((\?|#)[\w?&#=.]+)?""".r
  private val queryPart = """[\w/.]+\?([\w&=.]+)(#[\w.]*)?""".r
  private val queryParam = """([\w.]+)=([\w.]+)""".r
  private val fragmentPart = """[\w/?&=.]+#([\w.]*)""".r

  object Path {
    def unapply(relativeUrl: String): Option[List[String]] = {
      relativeUrl match {
        case hierarchicalPart(path, _*) => Some(path.split("/").filter(!_.isEmpty).toList)
        case _ => None
      }
    }
  }

  object Query {
    def unapply(relativeUrl: String): Option[List[(String, String)]] = {
      relativeUrl match {
        case queryPart(params, _*) =>
          Some(params.split("&").flatMap(queryParam.findFirstMatchIn(_).map(m => m.group(1) -> m.group(2))).toList)
        case _ => Some(List.empty)
      }
    }
  }

  object Fragment {
    def unapply(relativeUrl: String): Option[String] = {
      relativeUrl match {
        case fragmentPart(f) => Some(f)
        case _ => None
      }
    }
  }

  case class Url(scheme: String, authority: String, path: List[String], params: List[(String, String)] = List.empty, fragment: String = "") {
    def mkString: String = {
      val schemePart = if (scheme.isEmpty) "" else s"$scheme://"
      val pathPart = if (path.length == 0) "" else s"/${path.mkString("/")}"
      val queryPart = if (params.isEmpty) "" else s"?${params.map(p => s"${p._1}=${p._2}").mkString("&")}"
      val fragmentPart = if (fragment.isEmpty) "" else s"#$fragment"
      s"$schemePart$authority$pathPart$queryPart$fragmentPart"
    }
  }

  object Url {
    def unapply(url: String): Option[(String, String, String)] = {
      val (scheme, rest) = schemePart.unapplySeq(url).map(results => (results(0), results(1))).getOrElse(("", url))

      rest match {
        case authorityPartNoRest(auth) => Some(scheme, auth, "")
        case authorityPart(auth, hierPart) => Some(scheme, auth, hierPart)
        case _ => None
      }
    }

    def apply(scheme: String, authority: String, hierarchicalPart: String): Url = {
      val path = Path.unapply(hierarchicalPart).getOrElse(List.empty)
      val params = Query.unapply(hierarchicalPart).getOrElse(List.empty)
      val fragment = Fragment.unapply(hierarchicalPart).getOrElse("")

      Url(scheme, authority, path, params, fragment)
    }

    def apply(url: String): Url = {
      val (scheme, authority, hierarchicalPart) = unapply(url).getOrElse(("", "", ""))
      Url(scheme, authority, hierarchicalPart)
    }
  }

}
