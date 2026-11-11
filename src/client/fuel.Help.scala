                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                 ╭─────────╮╭───╮   ╭───╮╭─────────╮╭───╮                         ┃
┃                                 │   ╭─────╯│   │   │   ││   ╭─────╯│   │                         ┃
┃                                 │   ╰───╮  │   │   │   ││   ╰───╮  │   │                         ┃
┃                                 │   ╭───╯  │   │   │   ││   ╭───╯  │   │                         ┃
┃                                 │   │      │   ╰───╯   ││   ╰─────╮│   ╰─────╮                   ┃
┃                                 ╰───╯      ╰───────────╯╰─────────╯╰─────────╯                   ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃    FUEL, version 0.1.0.                                                                          ┃
┃    © Copyright 2026 Jon Pretty, Propensive OÜ.                                                   ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://propensive.dev/fuel/                                                              ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package fuel

import soundness.*

import environments.javaBaseEnvironment
import pyrocosm.{Channel, Peer, Swarm, Tool}

// What Fuel knows of the API it last compiled, for another tool on this machine to show beside
// a name — Flame's completions, say: each documented entity's key (`gossamer.Text#length`) and
// its `info` line, as plain text. A tool connects as a peer, over the `help` service a `help`
// in the configuration launches (on `help-port`, 8094 by default), and is sent the whole index
// at once; the connection then closes. A `fuel compile` publishes a new model, which the
// daemon's node announces on the swarm as an `api` event carrying the edition, so that a tool
// holding a copy of the index knows to ask again. Nothing is served through the hub: its bus
// floods every node and caps an event at 16 KiB, which is no vehicle for an index of
// hundreds of lines; the hub only tells a tool where this service is.
object Help:
  // The messages a help peer and its caller exchange: the index, sent once the handshake has
  // welcomed the caller. Only this layout must agree between the two tools; a mirror of it in
  // the caller derives the same fingerprint.
  enum Message:
    case Index(edition: Long, entries: List[Entry])

  case class Entry(key: Text, info: Text)

  val port: Int = 8094
  val protocol: Text = t"fuel-help"
  val event: Text = t"api"

  // Derived once: the schema, its fingerprint (the protocol the handshake names) and the codec.
  lazy val codec: Channel.Codec[Message] =
    import Channel.derivation.throwing
    val schema: Tels = Tels.tels[Message](protocol)

    Channel.Codec
      ( protocol, schema, message => Channel.encode(message, schema),
        data => Channel.decode[Message](data) )

  // The identity and token a listener needs, from the process's own environment: a service
  // runs for the daemon, outside any invocation. The token is the machine's own, which another
  // tool on this machine reads from the same file.
  private def identity: Optional[Peer.Identity] = safely(Peer.identity)

  // The index of the model `Server` holds, made once per edition: every entity with an `info`,
  // rendered to plain text through its own scope, so that a sigilled reference reads as a name.
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var cached: Optional[Message.Index] = Unset

  def index(): Message.Index =
    val edition: Long = Server.edition
    val current: Optional[Message.Index] =
      cached.let { index => if index.edition == edition then index else Unset }

    current.or:
      val api: Api = Server.api

      val entries: List[Entry] =
        api.entities.values.to(List).bind: (entity: Api.Entity) =>
          entity.doc.let(_.info).lay(Nil: List[Entry]): (info: Text) =>
            val scope: Scope =
              Scope.of(entity, entity.doc.let(_.imports).or(Nil), Server.standard)

            val plain: Text = Blocks.plain(Blocks.inline(info, Resolver(api, scope)))
            List(Entry(entity.key, plain))

      val made: Message.Index = Message.Index(edition, entries)
      cached = made
      made

  // Tells the swarm that the model has changed, with its edition: a tool holding the index
  // fetches it again.
  def announce(): Unit =
    Swarm.of(Fuel.name).let(_.publish(event, Server.edition.show.sysData))
    ()

  val service: Tool.Service = new Tool.Service:
    def keyword: Text = t"help"
    def portKeyword: Text = t"helpPort"
    def port: Int = Help.port

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var listener: Optional[Peer.Listener[Message]] = Unset

    def serve(port: Int, settings: Text => Optional[Text])(using monitor: Monitor, probate: Probate)
    :   Unit =

      identity.let: identity =>
        Peer.token.let: token =>
          val made: Peer.Listener[Message] =
            Peer.Listener[Message](Fuel.name, Fuel.version, codec, token, identity, Nil, () => Unset):
              session =>
                try session.send(index()) finally session.close()

          listener = made
          made.serve(port)

    def stop(): Unit = listener.let(_.stop())
