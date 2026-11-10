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
trait Strings6_2     extends StringBaseP with StringExtP
trait StringsAlgo6_2 extends Strings6_2 with StringAlgoP {
  override val stringtypes: StringP.StringTypes & StringP.StringAlgoTypes = new StringP.StringTypes with StringP.StringAlgoTypes {}
}
trait Strings7_0 extends Strings6_2 with StringSetGetFlagP with StringBitUnitP with StringLcsP {
  override val stringtypes: StringP.StringTypes & StringP.StringBitUnitTypes = new StringP.StringTypes with StringP.StringBitUnitTypes {}
}
trait Strings8_2 extends Strings7_0 with StringBitopExtendedP {
  override val stringtypes: StringP.StringTypes & StringP.StringBitopTypes & StringP.StringBitUnitTypes = new StringP.StringTypes
    with StringP.StringBitopTypes
    with StringP.StringBitUnitTypes {}
}
trait Strings8_4 extends Strings8_2 with StringSetIfEqP with StringSetCompareP with StringDigestAndDelExP with StringMSetExP {
  override val stringtypes: StringP.StringTypes & StringP.StringBitopTypes & StringP.StringBitUnitTypes & StringP.StringCompareTypes & StringP.StringIfEqTypes =
    new StringP.StringTypes
      with StringP.StringBitopTypes
      with StringP.StringBitUnitTypes
      with StringP.StringCompareTypes
      with StringP.StringIfEqTypes {}
}
trait Strings8_8 extends Strings8_4 with StringIncrExP {
  override val stringtypes: StringP.StringTypes & StringP.StringBitopTypes & StringP.StringBitUnitTypes & StringP.StringCompareTypes & StringP.StringIfEqTypes & StringP.StringIncrExTypes =
    new StringP.StringTypes
      with StringP.StringBitopTypes
      with StringP.StringBitUnitTypes
      with StringP.StringCompareTypes
      with StringP.StringIfEqTypes
      with StringP.StringIncrExTypes {}
}
trait StringsValkey8_1 extends Strings7_0 with StringSetIfEqP {
  override val stringtypes: StringP.StringTypes & StringP.StringBitUnitTypes & StringP.StringIfEqTypes = new StringP.StringTypes
    with StringP.StringBitUnitTypes
    with StringP.StringIfEqTypes {}
}
trait StringsValkey9_0 extends StringsValkey8_1 with StringDelIfEqP
trait StringsValkey9_1 extends StringsValkey9_0 with StringMSetExP

trait Keys6_2 extends KeyBaseP with KeyExtP with KeyCopyDbP with KeyMigrateP with KeyMigrateExtP with KeyObjectP {
  override val keytypes: KeyP.KeyTypes & KeyP.KeyObjectTypes & KeyP.KeyMigrateTypes = new KeyP.KeyTypes
    with KeyP.KeyObjectTypes
    with KeyP.KeyMigrateTypes {}
}
trait Keys7_0 extends Keys6_2 with KeyExpireConditionP with KeySortReadOnlyP {
  override val keytypes: KeyP.KeyTypes & KeyP.KeyObjectTypes & KeyP.KeyMigrateTypes & KeyP.KeyExpireTypes = new KeyP.KeyTypes
    with KeyP.KeyObjectTypes
    with KeyP.KeyMigrateTypes
    with KeyP.KeyExpireTypes {}
}
trait Keys7_2 extends Keys7_0 with KeyWaitAofP
trait Keys8_8 extends Keys7_2 {
  override val keytypes: KeyP.KeyArrayTypes & KeyP.KeyObjectTypes & KeyP.KeyMigrateTypes & KeyP.KeyExpireTypes =
    new KeyP.KeyArrayTypes with KeyP.KeyObjectTypes with KeyP.KeyMigrateTypes with KeyP.KeyExpireTypes {}
}

trait Hashes6_2 extends HashBaseP with HashExtP
trait Hashes7_4 extends Hashes6_2 with HashScanNoValuesP with HashFieldExpiryP {
  override val hashtypes: HashP.HashTypes & HashP.HashExpiryTypes & HashP.HashPersistTypes = new HashP.HashTypes
    with HashP.HashExpiryTypes
    with HashP.HashPersistTypes {}
}
trait Hashes8_0 extends Hashes7_4 with HashGetSetExpiryP with HashGetDelP {
  override val hashtypes: HashP.HashTypes & HashP.HashExpiryTypes & HashP.HashPersistTypes & HashP.HashFieldsConditionTypes =
    new HashP.HashTypes with HashP.HashExpiryTypes with HashP.HashPersistTypes with HashP.HashFieldsConditionTypes {}
}
trait Hashes8_10      extends Hashes8_0 with HashImportP
trait HashesValkey8_0 extends Hashes6_2 with HashScanNoValuesP
trait HashesValkey9_0 extends HashesValkey8_0 with HashFieldExpiryP with HashGetSetExpiryP {
  override val hashtypes: HashP.HashTypes & HashP.HashExpiryTypes & HashP.HashPersistTypes & HashP.HashFieldsConditionTypes =
    new HashP.HashTypes with HashP.HashExpiryTypes with HashP.HashPersistTypes with HashP.HashFieldsConditionTypes {}
}
trait HashesValkey9_1 extends HashesValkey9_0 with HashGetDelP with HashSetExKeyConditionP {
  override val hashtypes: HashP.HashTypes & HashP.HashExpiryTypes & HashP.HashPersistTypes & HashP.HashFieldsConditionTypes & HashP.HashKeyConditionTypes =
    new HashP.HashTypes
      with HashP.HashExpiryTypes
      with HashP.HashPersistTypes
      with HashP.HashFieldsConditionTypes
      with HashP.HashKeyConditionTypes {}
}

trait Lists6_2  extends ListBaseP with ListExtP
trait Lists7_0  extends Lists6_2 with ListMPopP with ListPopNilArrP
trait Lists8_10 extends Lists7_0 with ListMoveManyP {
  override val listtypes: ListP.ListTypes & ListP.ListMoveManyTypes = new ListP.ListTypes with ListP.ListMoveManyTypes {}
}

trait Sets6_2  extends SetBaseP with SetExtP
trait Sets7_0  extends Sets6_2 with SetInterCardP
trait Sets8_10 extends Sets7_0 with SetDiffCardP with SetUnionCardP {
  override val settypes: SetP.SetTypes & SetP.SetUnionCardTypes = new SetP.SetTypes with SetP.SetUnionCardTypes {}
}

trait SortedSets6_2 extends SortedSetBaseP with SortedSetExtP
trait SortedSets7_0 extends SortedSets6_2 with SortedSetInterCardP with SortedSetMPopP {
  override val sortedsettypes: SortedSetP.SortedSetTypes & SortedSetP.SortedSetMPopTypes = new SortedSetP.SortedSetTypes
    with SortedSetP.SortedSetMPopTypes {}
}
trait SortedSets7_2 extends SortedSets7_0 with SortedSetRankWithScoreP
trait SortedSets8_8 extends SortedSets7_2 with SortedSetAggregateCountP {
  override val sortedsettypes: SortedSetP.SortedSetAggregateCountTypes & SortedSetP.SortedSetMPopTypes =
    new SortedSetP.SortedSetAggregateCountTypes with SortedSetP.SortedSetMPopTypes {}
}
trait SortedSetsValkey8_0 extends SortedSets7_2 with SortedSetScanNoScoresP

trait Geo6_2       extends GeoBaseP with GeoExtP
trait GeoValkey9_0 extends Geo6_2 with GeoSearchPolygonP {
  override val geotypes: GeoP.GeoTypes & GeoP.GeoPolygonTypes = new GeoP.GeoTypes with GeoP.GeoPolygonTypes {}
}

trait Streams6_2 extends StreamBaseP with StreamGroupBaseP with StreamClaimAndInfoBaseP
trait Streams7_0
    extends Streams6_2
    with StreamClaimDeletedP
    with StreamGroupEntriesReadP
    with StreamSetIdEntriesP
    with StreamIdAtP
    with StreamInfo7_0P {
  override val streamtypes: StreamP.StreamTypes & StreamP.StreamIdAtTypes = new StreamP.StreamTypes with StreamP.StreamIdAtTypes {}
}
trait Streams7_2 extends Streams7_0 with StreamInfo7_2P
trait Streams8_2 extends Streams7_2 with StreamRefPolicyP {
  override val streamtypes: StreamP.StreamTypes & StreamP.StreamIdAtTypes & StreamP.StreamRefPolicyTypes = new StreamP.StreamTypes
    with StreamP.StreamIdAtTypes
    with StreamP.StreamRefPolicyTypes {}
}
trait Streams8_4 extends Streams8_2 with StreamReadGroupClaimP {
  override val streamtypes: StreamP.StreamTypes & StreamP.StreamIdAtTypes & StreamP.StreamRefPolicyTypes & StreamP.StreamClaimTypes =
    new StreamP.StreamTypes with StreamP.StreamIdAtTypes with StreamP.StreamRefPolicyTypes with StreamP.StreamClaimTypes {}
}
trait Streams8_6 extends Streams8_4 with StreamIdmpP {
  override val streamtypes: StreamP.StreamTypes & StreamP.StreamIdAtTypes & StreamP.StreamRefPolicyTypes & StreamP.StreamClaimTypes & StreamP.StreamIdmpTypes =
    new StreamP.StreamTypes
      with StreamP.StreamIdAtTypes
      with StreamP.StreamRefPolicyTypes
      with StreamP.StreamClaimTypes
      with StreamP.StreamIdmpTypes {}
}
trait Streams8_8 extends Streams8_6 with StreamNackP {
  override val streamtypes: StreamP.StreamTypes & StreamP.StreamIdAtTypes & StreamP.StreamRefPolicyTypes & StreamP.StreamClaimTypes & StreamP.StreamIdmpTypes & StreamP.StreamNackTypes =
    new StreamP.StreamTypes
      with StreamP.StreamIdAtTypes
      with StreamP.StreamRefPolicyTypes
      with StreamP.StreamClaimTypes
      with StreamP.StreamIdmpTypes
      with StreamP.StreamNackTypes {}
}
trait Streams8_10 extends Streams8_8 with StreamReadLimitsP {
  override val streamtypes: StreamP.StreamTypes & StreamP.StreamIdAtTypes & StreamP.StreamRefPolicyTypes & StreamP.StreamClaimTypes & StreamP.StreamIdmpTypes & StreamP.StreamNackTypes & StreamP.StreamReadLimitsTypes =
    new StreamP.StreamTypes
      with StreamP.StreamIdAtTypes
      with StreamP.StreamRefPolicyTypes
      with StreamP.StreamClaimTypes
      with StreamP.StreamIdmpTypes
      with StreamP.StreamNackTypes
      with StreamP.StreamReadLimitsTypes {}
}

// the builders each version's commands take, grouped like the commands
trait ClusterTypes8_2       extends ClusterP.ClusterBaseTypes with ClusterP.ClusterSlotStatsTypes
trait ClusterTypesValkey9_1 extends ClusterTypes8_2 with ClusterP.ClusterScanTypes
trait ServerTypes7_0
    extends ServerP.ServerBaseTypes
    with ServerP.CommandListTypes
    with ServerP.AclSelectorTypes
    with ServerP.ShutdownTypes
    with ServerP.InfoLatencyStatsTypes
trait ServerTypes8_0       extends ServerTypes7_0 with ServerP.InfoRedis8Types
trait ServerTypes8_6       extends ServerTypes8_0 with ServerP.HotKeysTypes with ServerP.InfoHotKeysTypes
trait ServerTypesValkey8_1 extends ServerTypes7_0 with ServerP.CommandLogTypes
trait ServerTypesValkey9_0 extends ServerTypesValkey8_1 with ServerP.ValkeyFilterTypes with ServerP.ValkeyShutdownTypes
trait ServerTypesValkey9_1 extends ServerTypesValkey9_0 with ServerP.AclDatabaseTypes with ServerP.InfoTlsTypes
trait ServerTypesDragonfly2_0
    extends ServerP.ServerTypes
    with ServerP.AclDragonflyTypes
    with ServerP.DragonflyShutdownTypes
    with ServerP.InfoLatencyStatsTypes

trait Cluster6_2 extends ClusterBaseP
trait Cluster7_0 extends Cluster6_2 with ClusterSlotsMetadataP with ClusterShardsP
trait Cluster7_2 extends Cluster7_0 with ClusterMyShardIdP
trait Cluster8_2 extends Cluster7_2 with ClusterSlotStatsP {
  override val clustertypes: ClusterTypes8_2 = new ClusterTypes8_2 {}
}
trait Cluster8_4       extends Cluster8_2 with ClusterSlotStatsMemoryP with ClusterMigrationP
trait ClusterValkey8_0 extends Cluster7_2 with ClusterSlotStatsP with ClusterSetSlotTimeoutP {
  override val clustertypes: ClusterTypes8_2 = new ClusterTypes8_2 {}
}
trait ClusterValkey9_0 extends ClusterValkey8_0 with ClusterReplicateNoOneP with ClusterSlotMigrationsP
trait ClusterValkey9_1 extends ClusterValkey9_0 with ClusterScanP with ClusterShardIdP {
  override val clustertypes: ClusterTypesValkey9_1 = new ClusterTypesValkey9_1 {}
}

/** The CLIENT and CONFIG commands, under `client` and `config` */
trait Client7_0       extends ClientCommands with ClientNoEvictCommands
trait Client7_2       extends Client7_0 with ClientNoTouchCommands
trait Client7_4       extends Client7_2 with ClientMaxAgeCommands
trait ClientValkey8_0 extends Client7_4 with ClientCapaCommands
trait ClientValkey8_1 extends ClientValkey8_0 with ClientImportSourceCommands with ClientListFilterCommands
trait ClientValkey9_0 extends ClientValkey8_1 with ClientValkeyFilterCommands
trait Config7_0       extends ConfigCommands with ConfigManyCommands

/** The LATENCY and COMMAND commands, under `latency` and `command` */
trait Latency7_0 extends LatencyCommands with LatencyHistogramCommands
trait Command7_0 extends CommandCommands with CommandListCommands with CommandInfo7_0Commands

/** The MODULE commands, under `module` */
trait Module7_0 extends ModuleCommands with ModuleLoadExCommands with ModuleList7_0Commands

/** The ACL commands, under `acl` */
trait Acl7_0       extends AclCommands with AclSelectorCommands
trait Acl7_2       extends Acl7_0 with AclLog7_2Commands
trait AclValkey9_1 extends Acl7_2 with AclDatabaseCommands

/** The SCRIPT commands, under `script` */
trait ScriptValkey8_0 extends ScriptCommands with ScriptShowCommands

/** The blocking commands, under `blocking` */
trait Blocking6_2  extends BListBaseP with BListExtP with BSortedSetBaseP with BStreamBaseP
trait Blocking7_0  extends Blocking6_2 with BListMPopP with BSortedSetMPopP
trait Blocking8_4  extends Blocking7_0 with BStreamReadGroupClaimP
trait Blocking8_10 extends Blocking8_4 with BListMoveManyP with BStreamReadLimitsP

trait Server6_2 extends ServerBaseP
trait Server7_0 extends Server6_2 with ServerShutdownAndInfoP {
  override lazy val client: Client7_0   = new Client7_0 {}
  override lazy val config: Config7_0   = new Config7_0 {}
  override lazy val latency: Latency7_0 = new Latency7_0 {}
  override lazy val command: Command7_0 = new Command7_0 {}
  override lazy val module: Module7_0   = new Module7_0 {}
  override lazy val acl: Acl7_0         = new Acl7_0 {}

  override val servertypes: ServerTypes7_0 = new ServerTypes7_0 {}
}
trait Server7_2 extends Server7_0 {
  override lazy val client: Client7_2 = new Client7_2 {}
  override lazy val acl: Acl7_2       = new Acl7_2 {}
}
trait Server7_4 extends Server7_2 {
  override lazy val client: Client7_4 = new Client7_4 {}
}
trait Server8_0 extends Server7_4 with ServerInfoRedis8P {
  override val servertypes: ServerTypes8_0 = new ServerTypes8_0 {}
}
trait Server8_6 extends Server8_0 with ServerHotKeysP {
  override val servertypes: ServerTypes8_6 = new ServerTypes8_6 {}
}
trait Server8_10 extends Server8_6 with ServerBackupP {
  override val slowlog: SlowLogCommands8_10 = new SlowLogCommands8_10 {}
}
trait ServerValkey8_0 extends Server7_2 {
  override lazy val client: ClientValkey8_0 = new ClientValkey8_0 {}
}
trait ServerValkey8_1 extends ServerValkey8_0 with ServerBgSaveCancelP with ServerCommandLogP {
  override lazy val client: ClientValkey8_1 = new ClientValkey8_1 {}

  override val servertypes: ServerTypesValkey8_1 = new ServerTypesValkey8_1 {}
}
trait ServerValkey9_0 extends ServerValkey8_1 with ServerShutdownSafeP {
  override lazy val client: ClientValkey9_0 = new ClientValkey9_0 {}

  override val servertypes: ServerTypesValkey9_0 = new ServerTypesValkey9_0 {}
}
trait ServerValkey9_1 extends ServerValkey9_0 with ServerInfoTlsP {
  override lazy val acl: AclValkey9_1 = new AclValkey9_1 {}

  override val servertypes: ServerTypesValkey9_1 = new ServerTypesValkey9_1 {}
}

trait Scripting6_2 extends ScriptingBaseP
trait Scripting7_0 extends Scripting6_2 with ScriptingFunctionsP {
  override val scriptingtypes: ScriptingP.ScriptingTypes & ScriptingP.ScriptingFunctionTypes = new ScriptingP.ScriptingTypes
    with ScriptingP.ScriptingFunctionTypes {}
}
trait ScriptingValkey8_0 extends Scripting7_0 {
  override lazy val script: ScriptValkey8_0 = new ScriptValkey8_0 {}
}

trait Connection6_2 extends ConnectionBaseP with ConnectionSwapDbP with ConnectionHello6_2P
trait Connection7_0 extends Connection6_2 with ConnectionHello7_0P

/** What Redis 6.2 has, which every version below builds on */
trait Base6_2
    extends Cluster6_2
    with Connection6_2
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
  val connection: Connection6_2 = new Connection6_2 {}
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

/** Redis 7.0 removed STRALGO, so the later versions go on from the base and not from this */
trait Redis6_2 extends Base6_2 with StringAlgoP {
  override val stringtypes: StringP.StringTypes & StringP.StringAlgoTypes = new StringP.StringTypes with StringP.StringAlgoTypes {}

  override val strings: StringsAlgo6_2 = new StringsAlgo6_2 {}
}
trait Redis7_0
    extends Base6_2
    with Cluster7_0
    with Connection7_0
    with Keys7_0
    with Lists7_0
    with PublishShardP
    with Scripting7_0
    with Server7_0
    with Sets7_0
    with SortedSets7_0
    with Streams7_0
    with Strings7_0 {
  override val blocking: Blocking7_0 = new Blocking7_0 {}

  override val cluster: Cluster7_0       = new Cluster7_0 {}
  override val connection: Connection7_0 = new Connection7_0 {}
  override val keys: Keys7_0             = new Keys7_0 {}
  override val lists: Lists7_0           = new Lists7_0 {}
  override val publish: PublishShardP    = new PublishShardP {}
  override val scripting: Scripting7_0   = new Scripting7_0 {}
  override val server: Server7_0         = new Server7_0 {}
  override val sets: Sets7_0             = new Sets7_0 {}
  override val sortedsets: SortedSets7_0 = new SortedSets7_0 {}
  override val streams: Streams7_0       = new Streams7_0 {}
  override val strings: Strings7_0       = new Strings7_0 {}
}
trait Redis7_2 extends Redis7_0 with Cluster7_2 with Keys7_2 with Server7_2 with SortedSets7_2 with Streams7_2 {
  override val streams: Streams7_2       = new Streams7_2 {}
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
trait Redis8_4 extends Redis8_2 with Cluster8_4 with Streams8_4 with Strings8_4 {
  override val blocking: Blocking8_4 = new Blocking8_4 {}

  override val cluster: Cluster8_4 = new Cluster8_4 {}
  override val streams: Streams8_4 = new Streams8_4 {}
  override val strings: Strings8_4 = new Strings8_4 {}
}
trait Redis8_6 extends Redis8_4 with Server8_6 with Streams8_6 {
  override val server: Server8_6   = new Server8_6 {}
  override val streams: Streams8_6 = new Streams8_6 {}
}
trait Redis8_8 extends Redis8_6 with ArrayP with Keys8_8 with SortedSets8_8 with Streams8_8 with Strings8_8 {
  val arrays: ArrayP = new ArrayP {}

  override val keys: Keys8_8             = new Keys8_8 {}
  override val sortedsets: SortedSets8_8 = new SortedSets8_8 {}
  override val streams: Streams8_8       = new Streams8_8 {}
  override val strings: Strings8_8       = new Strings8_8 {}
}
trait Redis8_10 extends Redis8_8 with Hashes8_10 with Lists8_10 with Server8_10 with Sets8_10 with Streams8_10 {
  override val blocking: Blocking8_10 = new Blocking8_10 {}

  override val hashmaps: Hashes8_10 = new Hashes8_10 {}
  override val lists: Lists8_10     = new Lists8_10 {}
  override val server: Server8_10   = new Server8_10 {}
  override val sets: Sets8_10       = new Sets8_10 {}
  override val streams: Streams8_10 = new Streams8_10 {}
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
trait Valkey9_0 extends Valkey8_1 with ClusterValkey9_0 with GeoValkey9_0 with HashesValkey9_0 with ServerValkey9_0 with StringsValkey9_0 {
  override val cluster: ClusterValkey9_0 = new ClusterValkey9_0 {}
  override val geo: GeoValkey9_0         = new GeoValkey9_0 {}
  override val hashmaps: HashesValkey9_0 = new HashesValkey9_0 {}
  override val server: ServerValkey9_0   = new ServerValkey9_0 {}
  override val strings: StringsValkey9_0 = new StringsValkey9_0 {}
}
trait Valkey9_1 extends Valkey9_0 with ClusterValkey9_1 with HashesValkey9_1 with ServerValkey9_1 with StringsValkey9_1 {
  override val cluster: ClusterValkey9_1 = new ClusterValkey9_1 {}
  override val hashmaps: HashesValkey9_1 = new HashesValkey9_1 {}
  override val server: ServerValkey9_1   = new ServerValkey9_1 {}
  override val strings: StringsValkey9_1 = new StringsValkey9_1 {}
}

/** Dragonfly, made of the features it accepts, as `COMMAND DOCS` doesn't say */
trait ClusterDragonfly2_0 extends ClusterCoreP with ClusterShardsCoreP
trait GeoDragonfly2_0     extends Geo6_2
trait HashesDragonfly2_0  extends HashBaseP with HashExtP with HashScanNoValuesP with HashFieldExpiryCoreP with HashGetSetExpiryP {
  override val hashtypes: HashP.HashTypes & HashP.HashExpiryTypes & HashP.HashFieldsConditionTypes = new HashP.HashTypes
    with HashP.HashExpiryTypes
    with HashP.HashFieldsConditionTypes {}
}
trait KeysDragonfly2_0 extends KeyBaseP with KeyExtP with KeyExpireConditionP with KeySortReadOnlyP {
  override val keytypes: KeyP.KeyTypes & KeyP.KeyExpireTypes = new KeyP.KeyTypes with KeyP.KeyExpireTypes {}
}
trait StreamsDragonfly2_0
    extends StreamBaseP
    with StreamGroupBaseP
    with StreamClaimAndInfoBaseP
    with StreamClaimDeletedP
    with StreamGroupEntriesReadP
    with StreamIdAtP {
  override val streamtypes: StreamP.StreamTypes & StreamP.StreamIdAtTypes = new StreamP.StreamTypes with StreamP.StreamIdAtTypes {}
}
trait StringsDragonfly2_0 extends Strings6_2 with StringSetGetFlagP with StringBitUnitP with StringDigestAndDelExP {
  override val stringtypes: StringP.StringTypes & StringP.StringBitUnitTypes & StringP.StringCompareTypes & StringP.StringIfEqTypes =
    new StringP.StringTypes with StringP.StringBitUnitTypes with StringP.StringCompareTypes with StringP.StringIfEqTypes {}
}
trait HyperLogLogDragonfly2_0 extends HyperLogLogBaseP with HyperLogLogExtP

trait ClientDragonfly2_0  extends ClientCoreCommands with ClientSetInfoCommands with ClientDragonflyKillCommands
trait LatencyDragonfly2_0 extends LatencyLatestCommands with LatencyHistogramCommands

trait ServerDragonfly2_0 extends ServerCoreP with ServerShutdownDragonflyP with ServerInfoManyP {
  override val servertypes: ServerTypesDragonfly2_0 = new ServerTypesDragonfly2_0 {}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ client.supported ++ config.supported ++ latency.supported ++ command.supported ++ acl.supported ++
      memory.supported ++ module.supported

  lazy val client: ClientDragonfly2_0   = new ClientDragonfly2_0 {}
  lazy val config: ConfigCommands       = new ConfigCommands {}
  lazy val latency: LatencyDragonfly2_0 = new LatencyDragonfly2_0 {}
  lazy val command: CommandCoreCommands = new CommandCoreCommands {}
  lazy val acl: AclDragonflyCommands    = new AclDragonflyCommands {}
  lazy val memory: MemoryCoreCommands   = new MemoryCoreCommands {}
  lazy val module: ModuleListCommands   = new ModuleListCommands {}
}

trait ScriptingDragonfly2_0 extends ScriptingReadOnlyP {
  override private[laserdisc] def supported: List[String] = super.supported ++ script.supported

  lazy val script: ScriptCoreCommands = new ScriptCoreCommands {}
}

trait Dragonfly2_0
    extends ClusterDragonfly2_0
    with ConnectionBaseP
    with DragonflyP
    with GeoDragonfly2_0
    with HashesDragonfly2_0
    with HyperLogLogDragonfly2_0
    with KeysDragonfly2_0
    with Lists7_0
    with PublishShardP
    with ScriptingDragonfly2_0
    with ServerDragonfly2_0
    with Sets7_0
    with SortedSets7_2
    with StreamsDragonfly2_0
    with StringsDragonfly2_0
    with TransactionP {
  val blocking: Blocking7_0 = new Blocking7_0 {}

  val cluster: ClusterDragonfly2_0         = new ClusterDragonfly2_0 {}
  val connection: ConnectionBaseP          = new ConnectionBaseP {}
  val dragonfly: DragonflyP                = new DragonflyP {}
  val geo: GeoDragonfly2_0                 = new GeoDragonfly2_0 {}
  val hashmaps: HashesDragonfly2_0         = new HashesDragonfly2_0 {}
  val hyperloglog: HyperLogLogDragonfly2_0 = new HyperLogLogDragonfly2_0 {}
  val keys: KeysDragonfly2_0               = new KeysDragonfly2_0 {}
  val lists: Lists7_0                      = new Lists7_0 {}
  val publish: PublishShardP               = new PublishShardP {}
  val scripting: ScriptingDragonfly2_0     = new ScriptingDragonfly2_0 {}
  val server: ServerDragonfly2_0           = new ServerDragonfly2_0 {}
  val sets: Sets7_0                        = new Sets7_0 {}
  val sortedsets: SortedSets7_2            = new SortedSets7_2 {}
  val streams: StreamsDragonfly2_0         = new StreamsDragonfly2_0 {}
  val strings: StringsDragonfly2_0         = new StringsDragonfly2_0 {}
  val transaction: TransactionP            = new TransactionP {}

  override private[laserdisc] def supported: List[String] = super.supported ++ blocking.supported
}

// what each import calls the replies that some versions send with more, mixed only into the imports
trait SlowLogReplies6_2 {
  final type LogEntry = ServerInspection.LogEntry6_2
  object LogEntry {
    def unapply(e: LogEntry): Some[(NonNegLong, NonNegLong, NonNegLong, Seq[String], String, String)] =
      Some((e.id, e.timestamp, e.microseconds, e.arguments, e.clientAddress, e.clientName))
  }
}
trait SlowLogReplies8_10 {
  final type LogEntry = ServerInspection.LogEntry8_10
  object LogEntry {
    def unapply(e: LogEntry): Some[(NonNegLong, NonNegLong, NonNegLong, Seq[String], String, String, NonNegLong)] =
      Some((e.id, e.timestamp, e.microseconds, e.arguments, e.clientAddress, e.clientName, e.totalArguments))
  }
}

trait AutoClaimedReplies6_2 {
  final type AutoClaimed[+A] = StreamP.AutoClaimed6_2[A]
  object AutoClaimed { def unapply[A](a: AutoClaimed[A]): Some[(StreamP.Id, Seq[A])] = Some((a.next, a.claimed)) }
}
trait AutoClaimedReplies7_0 {
  final type AutoClaimed[+A] = StreamP.AutoClaimed7_0[A]
  object AutoClaimed {
    def unapply[A](a: AutoClaimed[A]): Some[(StreamP.Id, Seq[A], Seq[StreamP.Id])] = Some((a.next, a.claimed, a.deleted))
  }
}
trait GroupInfoReplies6_2 {
  final type GroupInfo = StreamP.GroupInfo6_2
  object GroupInfo {
    def unapply(g: GroupInfo): Some[(Key, NonNegLong, NonNegLong, StreamP.Id)] = Some((g.name, g.consumers, g.pending, g.lastDeliveredId))
  }
}
trait GroupInfoReplies7_0 {
  final type GroupInfo = StreamP.GroupInfo7_0
  object GroupInfo {
    def unapply(g: GroupInfo): Some[(Key, NonNegLong, NonNegLong, StreamP.Id, Option[NonNegLong], Option[NonNegLong])] =
      Some((g.name, g.consumers, g.pending, g.lastDeliveredId, g.entriesRead, g.lag))
  }
}
trait ConsumerInfoReplies6_2 {
  final type ConsumerInfo = StreamP.ConsumerInfo6_2
  object ConsumerInfo { def unapply(c: ConsumerInfo): Some[(Key, NonNegLong, NonNegLong)] = Some((c.name, c.pending, c.idle)) }
}
trait ConsumerInfoReplies7_2 {
  final type ConsumerInfo = StreamP.ConsumerInfo7_2
  object ConsumerInfo {
    def unapply(c: ConsumerInfo): Some[(Key, NonNegLong, NonNegLong, Long)] = Some((c.name, c.pending, c.idle, c.inactive))
  }
}
trait StreamInfoFullReplies6_2 {
  final type StreamInfoFull[A] = StreamP.StreamInfoFull6_2[A]
  final type GroupInfoFull     = StreamP.GroupInfoFull6_2
  final type ConsumerInfoFull  = StreamP.ConsumerInfoFull6_2
}
trait StreamInfoFullReplies7_0 {
  final type StreamInfoFull[A] = StreamP.StreamInfoFull7_0[A]
  final type GroupInfoFull     = StreamP.GroupInfoFull7_0
  final type ConsumerInfoFull  = StreamP.ConsumerInfoFull6_2
}
trait StreamInfoFullReplies7_2 {
  final type StreamInfoFull[A] = StreamP.StreamInfoFull7_2[A]
  final type GroupInfoFull     = StreamP.GroupInfoFull7_2
  final type ConsumerInfoFull  = StreamP.ConsumerInfoFull7_2
}

trait AclUserReplies6_2 {
  final type AclUser = Acl.AclUser6_2
  object AclUser {
    def unapply(u: AclUser): Some[(Seq[String], Seq[String], String, String, String)] =
      Some((u.flags, u.passwords, u.commands, u.keys, u.channels))
  }
}
trait AclUserReplies7_0 {
  final type AclUser     = Acl.AclUser7_0
  final type AclSelector = Acl.AclSelector7_0
  object AclUser {
    def unapply(u: AclUser): Some[(Seq[String], Seq[String], String, String, String, Seq[AclSelector])] =
      Some((u.flags, u.passwords, u.commands, u.keys, u.channels, u.selectors))
  }
  object AclSelector { def unapply(s: AclSelector): Some[(String, String, String)] = Some((s.commands, s.keys, s.channels)) }
}
trait AclUserRepliesValkey9_1 {
  final type AclUser     = Acl.AclUserValkey9_1
  final type AclSelector = Acl.AclSelectorValkey9_1
  object AclUser {
    def unapply(u: AclUser): Some[(Seq[String], Seq[String], String, String, String, Seq[AclSelector], String)] =
      Some((u.flags, u.passwords, u.commands, u.keys, u.channels, u.selectors, u.databases))
  }
  object AclSelector {
    def unapply(s: AclSelector): Some[(String, String, String, String)] = Some((s.commands, s.keys, s.channels, s.databases))
  }
}
trait AclLogReplies6_2 {
  final type AclLogEntry = Acl.AclLogEntry6_2
  object AclLogEntry {
    def unapply(e: AclLogEntry): Some[(Long, String, String, String, String, Double, String)] =
      Some((e.count, e.reason, e.context, e.denied, e.username, e.ageSeconds, e.clientInfo))
  }
}
trait AclLogReplies7_2 {
  final type AclLogEntry = Acl.AclLogEntry7_2
  object AclLogEntry {
    def unapply(e: AclLogEntry): Some[(Long, String, String, String, String, Double, String, Long, Long, Long)] =
      Some((e.count, e.reason, e.context, e.denied, e.username, e.ageSeconds, e.clientInfo, e.entryId, e.createdAt, e.updatedAt))
  }
}

trait ClusterReplies6_2 {
  final type ClusterAddress  = ClusterP.Address6_2
  final type ClusterNode     = ClusterP.Node6_2
  final type ClusterNodes    = ClusterP.Nodes6_2
  final type ClusterSlotNode = ClusterP.SlotNode6_2
  final type ClusterSlotInfo = ClusterP.SlotInfo6_2
  final type ClusterSlots    = ClusterP.Slots6_2
  object ClusterNode {
    def unapply(n: ClusterNode): Some[
      (NodeId, Option[ClusterAddress], Seq[ClusterP.Flag], Option[NodeId], NonNegLong, NonNegLong, NonNegInt, ClusterP.LinkState, Seq[ClusterP.SlotType])
    ] = Some((n.id, n.address, n.flags, n.maybeMaster, n.ping, n.pong, n.configEpoch, n.link, n.slots))
  }
  object ClusterNodes    { def unapply(n: ClusterNodes): Some[Seq[ClusterNode]] = Some(n.nodes) }
  object ClusterSlotInfo {
    def unapply(i: ClusterSlotInfo): Some[(ClusterSlotNode, Seq[ClusterSlotNode])] = Some((i.master, i.replicas))
  }
  object ClusterSlots {
    def unapply(s: ClusterSlots): Some[Map[ClusterP.SlotType.Range, ClusterSlotInfo]] = Some(s.slots)
  }
  object ClusterAddress  { def unapply(a: ClusterAddress): Some[(Option[Host], Port, Port)] = Some((a.host, a.port, a.clusterPort)) }
  object ClusterSlotNode { def unapply(n: ClusterSlotNode): Some[(Option[Host], Port, NodeId)] = Some((n.host, n.port, n.nodeId))   }
}
trait ClusterReplies7_0 {
  final type ClusterAddress  = ClusterP.Address7_0
  final type ClusterNode     = ClusterP.Node7_0
  final type ClusterNodes    = ClusterP.Nodes7_0
  final type ClusterSlotNode = ClusterP.SlotNode7_0
  final type ClusterSlotInfo = ClusterP.SlotInfo7_0
  final type ClusterSlots    = ClusterP.Slots7_0
  object ClusterNode {
    def unapply(n: ClusterNode): Some[
      (NodeId, Option[ClusterAddress], Seq[ClusterP.Flag], Option[NodeId], NonNegLong, NonNegLong, NonNegInt, ClusterP.LinkState, Seq[ClusterP.SlotType])
    ] = Some((n.id, n.address, n.flags, n.maybeMaster, n.ping, n.pong, n.configEpoch, n.link, n.slots))
  }
  object ClusterNodes    { def unapply(n: ClusterNodes): Some[Seq[ClusterNode]] = Some(n.nodes) }
  object ClusterSlotInfo {
    def unapply(i: ClusterSlotInfo): Some[(ClusterSlotNode, Seq[ClusterSlotNode])] = Some((i.master, i.replicas))
  }
  object ClusterSlots {
    def unapply(s: ClusterSlots): Some[Map[ClusterP.SlotType.Range, ClusterSlotInfo]] = Some(s.slots)
  }
  object ClusterAddress {
    def unapply(a: ClusterAddress): Some[(Option[Host], Port, Port, Option[Host])] = Some((a.host, a.port, a.clusterPort, a.hostname))
  }
  object ClusterSlotNode {
    def unapply(n: ClusterSlotNode): Some[(Option[Host], Port, NodeId, Map[String, String])] = Some((n.host, n.port, n.nodeId, n.metadata))
  }
}
trait ShardReplies7_0 {
  final type ClusterShard = ClusterManagement.Shard7_0
  object ClusterShard {
    def unapply(s: ClusterShard): Some[(Seq[(Slot, Slot)], Seq[ClusterManagement.ShardNode])] = Some((s.slots, s.nodes))
  }
}
trait ShardRepliesValkey9_1 {
  final type ClusterShard = ClusterManagement.ShardValkey9_1
  object ClusterShard {
    def unapply(s: ClusterShard): Some[(Seq[(Slot, Slot)], Seq[ClusterManagement.ShardNode], String)] = Some((s.slots, s.nodes, s.id))
  }
}

trait CommandReplies6_2 {
  final type CommandInfo = ServerInspection.CommandInfo6_2
  final type ModuleInfo  = ServerInspection.ModuleInfo6_2
  object CommandInfo {
    def unapply(c: CommandInfo): Some[(String, Int, Seq[String], Int, Int, Int, Seq[String])] =
      Some((c.name, c.arity, c.flags, c.firstKey, c.lastKey, c.step, c.aclCategories))
  }
  object ModuleInfo { def unapply(m: ModuleInfo): Some[(String, Long)] = Some((m.name, m.version)) }
}
trait CommandReplies7_0 {
  final type CommandInfo = ServerInspection.CommandInfo7_0
  final type ModuleInfo  = ServerInspection.ModuleInfo7_0
  object CommandInfo {
    def unapply(c: CommandInfo): Some[
      (String, Int, Seq[String], Int, Int, Int, Seq[String], Seq[String], Seq[ServerInspection.KeySpecification], Seq[CommandInfo])
    ] = Some((c.name, c.arity, c.flags, c.firstKey, c.lastKey, c.step, c.aclCategories, c.tips, c.keySpecifications, c.subcommands))
  }
  object ModuleInfo {
    def unapply(m: ModuleInfo): Some[(String, Long, String, Seq[String])] = Some((m.name, m.version, m.path, m.arguments))
  }
}

trait HelloRepliesCore {
  final type Hello = ServerInspection.HelloCore
  object Hello {
    def unapply(h: Hello): Some[(String, String, Long, NonNegLong, String, String, Map[String, String])] =
      Some((h.server, h.version, h.protocol, h.id, h.mode, h.role, h.others))
  }
}
trait HelloReplies6_2 {
  final type Hello = ServerInspection.Hello6_2
  object Hello {
    def unapply(
        h: Hello
    ): Some[(String, String, Long, NonNegLong, String, String, Seq[ServerInspection.ModuleInfo6_2], Map[String, String])] =
      Some((h.server, h.version, h.protocol, h.id, h.mode, h.role, h.modules, h.others))
  }
}
trait HelloReplies7_0 {
  final type Hello = ServerInspection.Hello7_0
  object Hello {
    def unapply(
        h: Hello
    ): Some[(String, String, Long, NonNegLong, String, String, Seq[ServerInspection.ModuleInfo7_0], Map[String, String])] =
      Some((h.server, h.version, h.protocol, h.id, h.mode, h.role, h.modules, h.others))
  }
}

// the names of the replies whose shape changes along versions, as each import has them
trait RepliesDragonfly2_0
    extends SlowLogReplies6_2
    with AutoClaimedReplies7_0
    with GroupInfoReplies6_2
    with ConsumerInfoReplies6_2
    with StreamInfoFullReplies6_2
    with AclUserReplies6_2
    with AclLogReplies6_2
    with ClusterReplies6_2
    with ShardReplies7_0
    with CommandReplies6_2
    with HelloRepliesCore
trait Replies6_2
    extends SlowLogReplies6_2
    with AutoClaimedReplies6_2
    with GroupInfoReplies6_2
    with ConsumerInfoReplies6_2
    with StreamInfoFullReplies6_2
    with AclUserReplies6_2
    with AclLogReplies6_2
    with ClusterReplies6_2
    with ShardReplies7_0
    with CommandReplies6_2
    with HelloReplies6_2
trait Replies7_0
    extends SlowLogReplies6_2
    with AutoClaimedReplies7_0
    with GroupInfoReplies7_0
    with ConsumerInfoReplies6_2
    with StreamInfoFullReplies7_0
    with AclUserReplies7_0
    with AclLogReplies6_2
    with ClusterReplies7_0
    with ShardReplies7_0
    with CommandReplies7_0
    with HelloReplies7_0
trait Replies7_2
    extends SlowLogReplies6_2
    with AutoClaimedReplies7_0
    with GroupInfoReplies7_0
    with ConsumerInfoReplies7_2
    with StreamInfoFullReplies7_2
    with AclUserReplies7_0
    with AclLogReplies7_2
    with ClusterReplies7_0
    with ShardReplies7_0
    with CommandReplies7_0
    with HelloReplies7_0
trait Replies8_10
    extends SlowLogReplies8_10
    with AutoClaimedReplies7_0
    with GroupInfoReplies7_0
    with ConsumerInfoReplies7_2
    with StreamInfoFullReplies7_2
    with AclUserReplies7_0
    with AclLogReplies7_2
    with ClusterReplies7_0
    with ShardReplies7_0
    with CommandReplies7_0
    with HelloReplies7_0
trait RepliesValkey9_1
    extends SlowLogReplies6_2
    with AutoClaimedReplies7_0
    with GroupInfoReplies7_0
    with ConsumerInfoReplies7_2
    with StreamInfoFullReplies7_2
    with AclUserRepliesValkey9_1
    with AclLogReplies7_2
    with ClusterReplies7_0
    with ShardRepliesValkey9_1
    with CommandReplies7_0
    with HelloReplies7_0

object dragonfly_2_0 extends Dragonfly2_0 with RepliesDragonfly2_0

object redis_6_2  extends Redis6_2 with Replies6_2
object redis_7_0  extends Redis7_0 with Replies7_0
object redis_7_2  extends Redis7_2 with Replies7_2
object redis_7_4  extends Redis7_4 with Replies7_2
object redis_8_0  extends Redis8_0 with Replies7_2
object redis_8_2  extends Redis8_2 with Replies7_2
object redis_8_4  extends Redis8_4 with Replies7_2
object redis_8_6  extends Redis8_6 with Replies7_2
object redis_8_8  extends Redis8_8 with Replies7_2
object redis_8_10 extends Redis8_10 with Replies8_10
object valkey_7_2 extends Valkey7_2 with Replies7_2
object valkey_8_0 extends Valkey8_0 with Replies7_2
object valkey_8_1 extends Valkey8_1 with Replies7_2
object valkey_9_0 extends Valkey9_0 with Replies7_2
object valkey_9_1 extends Valkey9_1 with RepliesValkey9_1
