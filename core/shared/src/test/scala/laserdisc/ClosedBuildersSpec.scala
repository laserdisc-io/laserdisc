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

package elsewhere

/** Outside laserdisc, a builder only some versions take is only reachable through the import of a version that takes it */
final class ClosedBuildersSpec extends munit.FunSuite {
  test("the builders of later versions can't be reached around the imports") {
    assert(compileErrors("""laserdisc.protocol.StreamP.Idmp.auto(laserdisc.Key("p"))""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_8_6.streamtypes.StreamIdmp.auto(laserdisc.Key("p"))""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.StreamP.IdAt(laserdisc.NonNegLong(1L))""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_7_0.streamtypes.StreamIdAt(laserdisc.NonNegLong(1L))""").isEmpty)
    assert(compileErrors("""new laserdisc.protocol.StreamP.IdAt(laserdisc.NonNegLong(1L))""").nonEmpty)
    assert(compileErrors("""laserdisc.protocol.StreamP.ReadLimits.maxCount(laserdisc.PosLong(1L))""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_8_10.streamtypes.StreamReadLimits.maxCount(laserdisc.PosLong(1L))""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.KeyP.ExpireCondition.nx""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_7_0.keytypes.KeyExpireCondition.nx""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.HashP.FieldsCondition.fnx""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_8_0.hashtypes.HashFieldsCondition.fnx""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.StringP.Compare.ifDeq("d")""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_8_4.stringtypes.compare.ifDeq("d")""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.StringP.IfEq(1)""").nonEmpty)
    assert(compileErrors("""laserdisc.valkey_8_1.stringtypes.ifEq(1)""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.ServerInspection.HotKeysMetrics.cpu""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_8_6.servertypes.hotKeysMetrics.cpu""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.ClusterManagement.ScanCursor.start""").nonEmpty)
    assert(compileErrors("""laserdisc.valkey_9_1.clustertypes.ClusterScanCursor.start""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.Acl.AclRulesWithDatabases.none""").nonEmpty)
    assert(compileErrors("""laserdisc.valkey_9_1.servertypes.aclDatabaseRules""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.ScriptingP.Replace""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_7_0.scriptingtypes.ScriptingReplace""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.SetP.Approx.approx""").nonEmpty)
    assert(compileErrors("""laserdisc.redis_8_10.settypes.approx""").isEmpty)
    assert(compileErrors("""laserdisc.protocol.ServerP.DragonflyShutdownOption.now""").nonEmpty)
    assert(compileErrors("""laserdisc.protocol.StringP.BitwiseOf.diff""").nonEmpty)
    assert(compileErrors("""laserdisc.protocol.SortedSetP.AggregateOf.count""").nonEmpty)
    assert(compileErrors("""laserdisc.protocol.ServerP.InfoSections.keysizes""").nonEmpty)
    assert(compileErrors("""laserdisc.protocol.KeyP.TypeOf.array""").nonEmpty)
    assert(compileErrors("""laserdisc.dragonfly_2_0.servertypes.dragonflyShutdownOption.now""").isEmpty)
  }
}
