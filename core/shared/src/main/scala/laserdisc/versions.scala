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

import protocol.*

/** Each family's commands by version, for an import of one family only, like `laserdisc.redis_7_0.streams.*`. A new trait only where the
  * family changed. The versions below are made of these, so a version and its families can't disagree
  */
trait Strings6_2       extends StringBaseP with StringExtP
trait Strings7_0       extends Strings6_2 with StringSetGetFlagP with StringBitUnitP with StringLcsP
trait Strings8_2       extends Strings7_0 with StringBitopExtendedP
trait Strings8_4       extends Strings8_2 with StringSetIfEqP with StringSetCompareP with StringDigestAndDelExP with StringMSetExP
trait Strings8_8       extends Strings8_4 with StringIncrExP
trait StringsValkey8_1 extends Strings7_0 with StringSetIfEqP
trait StringsValkey9_0 extends StringsValkey8_1 with StringDelIfEqP
trait StringsValkey9_1 extends StringsValkey9_0 with StringMSetExP

trait Keys6_2 extends KeyBaseP with KeyExtP
trait Keys7_0 extends Keys6_2 with KeyExpireConditionP with KeySortReadOnlyP
trait Keys7_2 extends Keys7_0 with KeyWaitAofP

trait Hashes6_2       extends HashBaseP with HashExtP
trait Hashes7_4       extends Hashes6_2 with HashScanNoValuesP with HashFieldExpiryP
trait Hashes8_0       extends Hashes7_4 with HashGetSetExpiryP with HashGetDelP
trait HashesValkey8_0 extends Hashes6_2 with HashScanNoValuesP
trait HashesValkey9_0 extends HashesValkey8_0 with HashFieldExpiryP with HashGetSetExpiryP
trait HashesValkey9_1 extends HashesValkey9_0 with HashGetDelP with HashSetExKeyConditionP

trait Lists6_2  extends ListBaseP with ListExtP
trait Lists7_0  extends Lists6_2 with ListMPopP with ListPopNilArrP
trait Lists8_10 extends Lists7_0 with ListMoveManyP

trait Sets6_2  extends SetBaseP with SetExtP
trait Sets7_0  extends Sets6_2 with SetInterCardP
trait Sets8_10 extends Sets7_0 with SetDiffCardP with SetUnionCardP

trait SortedSets6_2       extends SortedSetBaseP with SortedSetExtP
trait SortedSets7_0       extends SortedSets6_2 with SortedSetInterCardP with SortedSetMPopP
trait SortedSets7_2       extends SortedSets7_0 with SortedSetRankWithScoreP
trait SortedSets8_8       extends SortedSets7_2 with SortedSetAggregateCountP
trait SortedSetsValkey8_0 extends SortedSets7_2 with SortedSetScanNoScoresP

trait Geo6_2       extends GeoBaseP with GeoExtP
trait GeoValkey9_0 extends Geo6_2 with GeoSearchPolygonP

trait Streams6_2 extends StreamBaseP with StreamGroupBaseP with StreamClaimAndInfoBaseP
trait Streams7_0 extends Streams6_2 with StreamClaimDeletedP with StreamGroupEntriesReadP with StreamSetIdEntriesP
trait Streams8_2 extends Streams7_0 with StreamRefPolicyP
trait Streams8_6 extends Streams8_2 with StreamIdmpP

trait Cluster6_2       extends ClusterBaseP
trait Cluster7_0       extends Cluster6_2 with ClusterSlotsMetadataP with ClusterShardsP
trait Cluster7_2       extends Cluster7_0 with ClusterMyShardIdP
trait Cluster8_2       extends Cluster7_2 with ClusterSlotStatsP
trait Cluster8_4       extends Cluster8_2 with ClusterMigrationP
trait ClusterValkey8_0 extends Cluster7_2 with ClusterSlotStatsP with ClusterSetSlotTimeoutP
trait ClusterValkey9_0 extends ClusterValkey8_0 with ClusterReplicateNoOneP with ClusterSlotMigrationsP

/** The CLIENT and CONFIG commands, under `client` and `config` */
trait Client7_0 extends ClientCommands with ClientNoEvictCommands
trait Client7_2 extends Client7_0 with ClientNoTouchCommands
trait Client7_4 extends Client7_2 with ClientMaxAgeCommands
trait Config7_0 extends ConfigCommands with ConfigManyCommands

/** The LATENCY and COMMAND commands, under `latency` and `command` */
trait Latency7_0 extends LatencyCommands with LatencyHistogramCommands
trait Command7_0 extends CommandCommands with CommandListCommands

/** The ACL commands, under `acl` */
trait Acl7_0 extends AclCommands with AclSelectorCommands

/** The SCRIPT commands, under `script` */
trait ScriptValkey8_0 extends ScriptCommands with ScriptShowCommands

/** The blocking commands, under `blocking` */
trait Blocking6_2  extends BListBaseP with BListExtP with BSortedSetBaseP with BStreamBaseP
trait Blocking7_0  extends Blocking6_2 with BListMPopP with BSortedSetMPopP
trait Blocking8_10 extends Blocking7_0 with BListMoveManyP

trait Server6_2 extends ServerBaseP
trait Server7_0 extends Server6_2 with ServerShutdownAndInfoP {
  override lazy val client: Client7_0   = new Client7_0 {}
  override lazy val config: Config7_0   = new Config7_0 {}
  override lazy val latency: Latency7_0 = new Latency7_0 {}
  override lazy val command: Command7_0 = new Command7_0 {}
  override lazy val acl: Acl7_0         = new Acl7_0 {}
}
trait Server7_2 extends Server7_0 {
  override lazy val client: Client7_2 = new Client7_2 {}
}
trait Server7_4 extends Server7_2 {
  override lazy val client: Client7_4 = new Client7_4 {}
}
trait Server8_0       extends Server7_4 with ServerInfoRedis8P
trait Server8_6       extends Server8_0 with ServerHotKeysP
trait Server8_10      extends Server8_6 with ServerBackupP
trait ServerValkey8_0 extends Server7_2 {
  override lazy val client: Client7_4 = new Client7_4 {}
}
trait ServerValkey8_1 extends ServerValkey8_0 with ServerBgSaveCancelP with ServerCommandLogP
trait ServerValkey9_1 extends ServerValkey8_1 with ServerInfoTlsP

trait Scripting6_2       extends ScriptingBaseP
trait Scripting7_0       extends Scripting6_2 with ScriptingFunctionsP
trait ScriptingValkey8_0 extends Scripting7_0 {
  override lazy val script: ScriptValkey8_0 = new ScriptValkey8_0 {}
}

/** What Redis 6.2 has, which every version below builds on */
trait Base6_2
    extends Cluster6_2
    with ConnectionP
    with Geo6_2
    with Hashes6_2
    with HyperLogLogP
    with Keys6_2
    with Lists6_2
    with PublishP
    with Scripting6_2
    with Server6_2
    with Sets6_2
    with SortedSets6_2
    with Streams6_2
    with Strings6_2
    with TransactionP {
  val blocking: Blocking6_2 = new Blocking6_2 {}

  val cluster: Cluster6_2       = new Cluster6_2 {}
  val connection: ConnectionP   = new ConnectionP {}
  val geo: Geo6_2               = new Geo6_2 {}
  val hashmaps: Hashes6_2       = new Hashes6_2 {}
  val hyperloglog: HyperLogLogP = new HyperLogLogP {}
  val keys: Keys6_2             = new Keys6_2 {}
  val lists: Lists6_2           = new Lists6_2 {}
  val publish: PublishP         = new PublishP {}
  val scripting: Scripting6_2   = new Scripting6_2 {}
  val server: Server6_2         = new Server6_2 {}
  val sets: Sets6_2             = new Sets6_2 {}
  val sortedsets: SortedSets6_2 = new SortedSets6_2 {}
  val streams: Streams6_2       = new Streams6_2 {}
  val strings: Strings6_2       = new Strings6_2 {}
  val transaction: TransactionP = new TransactionP {}

  override private[laserdisc] def supported: List[String] = super.supported ++ blocking.supported
}

trait Redis6_2 extends Base6_2
trait Redis7_0
    extends Redis6_2
    with Cluster7_0
    with Keys7_0
    with Lists7_0
    with Scripting7_0
    with Server7_0
    with Sets7_0
    with SortedSets7_0
    with Streams7_0
    with Strings7_0 {
  override val blocking: Blocking7_0 = new Blocking7_0 {}

  override val cluster: Cluster7_0       = new Cluster7_0 {}
  override val keys: Keys7_0             = new Keys7_0 {}
  override val lists: Lists7_0           = new Lists7_0 {}
  override val scripting: Scripting7_0   = new Scripting7_0 {}
  override val server: Server7_0         = new Server7_0 {}
  override val sets: Sets7_0             = new Sets7_0 {}
  override val sortedsets: SortedSets7_0 = new SortedSets7_0 {}
  override val streams: Streams7_0       = new Streams7_0 {}
  override val strings: Strings7_0       = new Strings7_0 {}
}
trait Redis7_2 extends Redis7_0 with Cluster7_2 with Keys7_2 with Server7_2 with SortedSets7_2 {
  override val cluster: Cluster7_2       = new Cluster7_2 {}
  override val keys: Keys7_2             = new Keys7_2 {}
  override val server: Server7_2         = new Server7_2 {}
  override val sortedsets: SortedSets7_2 = new SortedSets7_2 {}
}
trait Redis7_4 extends Redis7_2 with Hashes7_4 with Server7_4 {
  override val hashmaps: Hashes7_4 = new Hashes7_4 {}
  override val server: Server7_4   = new Server7_4 {}
}
trait Redis8_0 extends Redis7_4 with Hashes8_0 with Server8_0 {
  override val hashmaps: Hashes8_0 = new Hashes8_0 {}
  override val server: Server8_0   = new Server8_0 {}
}
trait Redis8_2 extends Redis8_0 with Cluster8_2 with Streams8_2 with Strings8_2 {
  override val cluster: Cluster8_2 = new Cluster8_2 {}
  override val streams: Streams8_2 = new Streams8_2 {}
  override val strings: Strings8_2 = new Strings8_2 {}
}
trait Redis8_4 extends Redis8_2 with Cluster8_4 with Strings8_4 {
  override val cluster: Cluster8_4 = new Cluster8_4 {}
  override val strings: Strings8_4 = new Strings8_4 {}
}
trait Redis8_6 extends Redis8_4 with Server8_6 with Streams8_6 {
  override val server: Server8_6   = new Server8_6 {}
  override val streams: Streams8_6 = new Streams8_6 {}
}
trait Redis8_8 extends Redis8_6 with SortedSets8_8 with Strings8_8 {
  override val sortedsets: SortedSets8_8 = new SortedSets8_8 {}
  override val strings: Strings8_8       = new Strings8_8 {}
}
trait Redis8_10 extends Redis8_8 with Lists8_10 with Server8_10 with Sets8_10 {
  override val blocking: Blocking8_10 = new Blocking8_10 {}

  override val lists: Lists8_10   = new Lists8_10 {}
  override val server: Server8_10 = new Server8_10 {}
  override val sets: Sets8_10     = new Sets8_10 {}
}

/** Valkey started from Redis 7.2 */
trait Valkey7_2 extends Redis7_2
trait Valkey8_0
    extends Valkey7_2
    with ClusterValkey8_0
    with HashesValkey8_0
    with ScriptingValkey8_0
    with ServerValkey8_0
    with SortedSetsValkey8_0 {
  override val cluster: ClusterValkey8_0       = new ClusterValkey8_0 {}
  override val hashmaps: HashesValkey8_0       = new HashesValkey8_0 {}
  override val scripting: ScriptingValkey8_0   = new ScriptingValkey8_0 {}
  override val server: ServerValkey8_0         = new ServerValkey8_0 {}
  override val sortedsets: SortedSetsValkey8_0 = new SortedSetsValkey8_0 {}
}
trait Valkey8_1 extends Valkey8_0 with ServerValkey8_1 with StringsValkey8_1 {
  override val server: ServerValkey8_1   = new ServerValkey8_1 {}
  override val strings: StringsValkey8_1 = new StringsValkey8_1 {}
}
trait Valkey9_0 extends Valkey8_1 with ClusterValkey9_0 with GeoValkey9_0 with HashesValkey9_0 with StringsValkey9_0 {
  override val cluster: ClusterValkey9_0 = new ClusterValkey9_0 {}
  override val geo: GeoValkey9_0         = new GeoValkey9_0 {}
  override val hashmaps: HashesValkey9_0 = new HashesValkey9_0 {}
  override val strings: StringsValkey9_0 = new StringsValkey9_0 {}
}
trait Valkey9_1 extends Valkey9_0 with HashesValkey9_1 with ServerValkey9_1 with StringsValkey9_1 {
  override val hashmaps: HashesValkey9_1 = new HashesValkey9_1 {}
  override val server: ServerValkey9_1   = new ServerValkey9_1 {}
  override val strings: StringsValkey9_1 = new StringsValkey9_1 {}
}

object redis_6_2  extends Redis6_2
object redis_7_0  extends Redis7_0
object redis_7_2  extends Redis7_2
object redis_7_4  extends Redis7_4
object redis_8_0  extends Redis8_0
object redis_8_2  extends Redis8_2
object redis_8_4  extends Redis8_4
object redis_8_6  extends Redis8_6
object redis_8_8  extends Redis8_8
object redis_8_10 extends Redis8_10

object valkey_7_2 extends Valkey7_2
object valkey_8_0 extends Valkey8_0
object valkey_8_1 extends Valkey8_1
object valkey_9_0 extends Valkey9_0
object valkey_9_1 extends Valkey9_1
