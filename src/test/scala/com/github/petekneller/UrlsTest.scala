package com.github.petekneller

import org.scalatest.{Matchers, FunSuite}

class UrlsTest extends FunSuite with Matchers {

  import urls._

  test("url can be decomposed") {

    val Url("http", "foo.com", "") = "http://foo.com"

    val Url("https", "foo.com", "/bar") = "https://foo.com/bar"

    val Url("https", "foo.com", "/bar?baz=1&quux=2") = "https://foo.com/bar?baz=1&quux=2"

    val Url("https", "foo.com", "/bar?baz=1&quux=2#twiddle") = "https://foo.com/bar?baz=1&quux=2#twiddle"

    val Url("https", "foo.com", "/bar#twiddle") = "https://foo.com/bar#twiddle"

    val Url("", "foo.com", "/bar") = "foo.com/bar"
  }

  test("path can be extracted from the relative part") {

    val Path("foo" :: "bar" :: "baz" :: Nil) = "/foo/bar/baz"

    val Path("foo" :: Nil) = "/foo?bar=baz"

    val Path("foo" :: Nil) = "/foo#bar"

    val Path("foo" :: Nil) = "/foo?bar=1#baz"
  }

  test("query part can be extracted from the relative part") {

    val QueryParams = "bar" -> "1" :: "baz" -> "2" :: Nil

    val Query(QueryParams) = "/foo?bar=1&baz=2"

    val Query(QueryParams) = "/foo?bar=1&baz=2#quux"

    val Query(Nil) = "/foo"
  }

  test("fragment can be extracted from the relative part") {

    val Fragment("bar") = "/foo#bar"

    val Fragment("") = "/foo/#"

    val Fragment("baz") = "/foo?bar=1#baz"
  }

  test("Url can be converted to a valid url string") {

    val url1 = Url("http", "foo.com", List("bar"))
    url1.mkString should be("http://foo.com/bar")

    val url2 = Url("", "foo.com", List("bar"))
    url2.mkString should be("foo.com/bar")

    val url3 = Url("", "foo.com", List("bar"), List("baz" -> "1", "quux" -> "2"))
    url3.mkString should be("foo.com/bar?baz=1&quux=2")

    val url4 = Url("", "foo.com", List("bar"), List.empty, "baz")
    url4.mkString should be("foo.com/bar#baz")

    val url5 = Url("https", "foo.com", List("bar", "baz"), List("wibble" -> "1", "wobble" -> "2"), "wubble")
    url5.mkString should be("https://foo.com/bar/baz?wibble=1&wobble=2#wubble")
  }

  test("ordering of parameters is preserved") {
    val original: String = "foo.com/bar?baz=1&quux=2"
    val Url(_, _, Query(params)) = original
    params should be ("baz" -> "1" :: "quux" -> "2" :: Nil)

    Url("", "foo.com", "bar" :: Nil, params).mkString should be (original)
  }

  // user/password parts?

  // invalid chars?

}
