/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package fs2

import laserdisc.auto.*

final class RedisGeoClientSpec extends GeoClientSpec(6379, "redis")
final class KeyDbGeoClientSpec extends GeoClientSpec(6380, "keyDb")

final class ValkeyGeoClientSpec extends GeoClientSpec(6381, "valkey") {
  test("a valkey client searches inside a polygon with the commands of Valkey 9.1") {
    import laserdisc.valkey_9_1.*
    import geotypes.*

    send(del("g:p", "g:q"))
    send(
      geoadd("g:p", OneOrMore.unsafeFrom(List(GeoPosition("Palermo", 38.115556, 13.361389), GeoPosition("Catania", 37.502669, 15.087269))))
    )

    val westOfCatania =
      GeoPolygon(GeoCoordinates(37.0, 12.0), GeoCoordinates(37.0, 14.0), GeoCoordinates(39.0, 14.0), GeoCoordinates(39.0, 12.0))
    assertEquals(send(geosearch("g:p", westOfCatania)), Seq(Key("Palermo")))
    assertEquals(send(geosearchstore("g:p", "g:q", westOfCatania, Direction.asc)).value, 1)
  }
}

abstract class GeoClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  test(s"a $dest client stops GEORADIUS at the first matches with ANY") {
    import laserdisc.redis_6_2.*
    import geotypes.*

    send(del("g:a", "g:b"))
    send(
      geoadd("g:a", OneOrMore.unsafeFrom(List(GeoPosition("Palermo", 38.115556, 13.361389), GeoPosition("Catania", 37.502669, 15.087269))))
    )

    val at  = GeoCoordinates(37.5, 15.0)
    val any = GeoCount.any(1)
    assertEquals(send(georadius("g:a", at, 300.0, GeoUnit.kilometers, any)).size, 1)
    assertEquals(send(georadius("g:a", at, 300.0, GeoUnit.kilometers, any, Direction.asc)).size, 1)
    assertEquals(send(georadius("g:a", "Palermo", 300.0, GeoUnit.kilometers, any)).size, 1)
    assertEquals(send(georadius("g:a", at, 300.0, GeoUnit.kilometers, GeoCount.closest(1), Direction.asc)), Seq(Key("Catania")))
    assertEquals(send(ro.georadius("g:a", at, 300.0, GeoUnit.kilometers, any)).size, 1)
    assertEquals(send(ro.georadius("g:a", "Palermo", 300.0, GeoUnit.kilometers, any)).size, 1)
    send(del("g:a", "g:b"))
  }

  test(s"a $dest client adds with NX, XX and CH, and searches by radius and by box") {
    import laserdisc.redis_6_2.*
    import geotypes.*

    send(del("g:s", "g:t"))
    val sicily = OneOrMore.unsafeFrom(List(GeoPosition("Palermo", 38.115556, 13.361389), GeoPosition("Catania", 37.502669, 15.087269)))
    assertEquals(send(geoadd("g:s", sicily)).value, 2)
    assertEquals(send(geoadd("g:s", GeoFlag.nx, OneOrMore.unsafeFrom(List(GeoPosition("Palermo", 38.0, 13.0))))).value, 0)
    assertEquals(send(geoaddch("g:s", GeoFlag.xx, OneOrMore.unsafeFrom(List(GeoPosition("Palermo", 38.1, 13.4))))).value, 1)

    val nearCatania = GeoArea.radius(GeoCenter.coordinates(GeoCoordinates(37.5, 15.0)), 200.0, GeoUnit.kilometers)
    assertEquals(send(geosearch("g:s", nearCatania, Direction.asc)), Seq(Key("Catania"), Key("Palermo")))
    assertEquals(send(geosearch("g:s", nearCatania, Direction.asc, GeoCount.closest(1))), Seq(Key("Catania")))
    assertEquals(send(geosearch("g:s", nearCatania, GeoCount.any(1))).size, 1)
    assertEquals(
      send(geosearch("g:s", nearCatania, Direction.asc, GeoCount.closest(1), GeoRadiusMode.distance)).map(_.key),
      Seq(Key("Catania"))
    )

    val aroundPalermo = GeoArea.box(GeoCenter.member("Palermo"), 400.0, 400.0, GeoUnit.kilometers)
    assertEquals(send(geosearchstore("g:s", "g:t", aroundPalermo, GeoStoreDistances)).value, 2)
    assertEquals(send(geosearchstore("g:s", "g:t", aroundPalermo, Direction.desc, GeoCount.closest(1))).value, 1)
  }
}
