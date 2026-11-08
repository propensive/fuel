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

import java.nio.file as jnf

import scala.collection.immutable as sci

import soundness.*

import alphabets.hexLowerCase
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import filesystemBackends.javaBaseFilesystem
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import providers.javaBaseProvider

// Montreal Forced Aligner, installed and run by fuel: a recording of a presentation's
// transcript is aligned against the transcript, and the alignment — the time each word is
// spoken — times the presentation's autoplay. MFA is published only as conda-forge packages
// (Kaldi's binaries among them), so fuel installs it through micromamba, a single static
// executable which puts conda-forge packages in a directory with no Anaconda, no shell set-up
// and no activation: everything lives under `$XDG_CACHE_HOME/fuel/mfa`, and is installed on
// first use, once. An alignment is kept in the project's cache by a digest of the recording
// and the transcript, so a presentation is aligned again only when either changes.
object Aligner:
  // The micromamba release fuel fetches, pinned so the download is reproducible; the MFA it
  // installs, pinned below 4.0, whose `mfa align` has another signature; and the English
  // dictionary and acoustic model it aligns with.
  val micromambaVersion: Text = t"2.10.0-0"
  val mfaSpec: Text = t"montreal-forced-aligner>=3.4,<4"
  val model: Text = t"english_mfa"

  // Where everything is kept: the micromamba executable, the environment it makes, MFA's
  // root (its models, and its own working files) and micromamba's package cache.
  case class Home(path: Text):
    def micromamba: Text = t"$path/bin/micromamba"
    def env: Text = t"$path/env"
    def root: Text = t"$path/root"
    def mamba: Text = t"$path/mamba"
    def mfa: Text = t"$env/bin/mfa"

  def home(using Environment): Optional[Home] =
    Fuel.cacheDirectory.let { (directory: Path on Linux) => Home(t"${directory.encode}/mfa") }

  // micromamba's name for the running JVM's platform, as its releases name their executables;
  // `Unset` where there is none.
  def platform(os: Text, arch: Text): Optional[Text] =
    val system: Optional[Text] =
      if os.lower.contains(t"mac") || os.lower.contains(t"darwin") then t"osx"
      else if os.lower.contains(t"linux") then t"linux"
      else Unset

    val architecture: Optional[Text] = arch.lower match
      case t"x86_64" | t"amd64"  => t"64"
      case t"aarch64" | t"arm64" => if system == t"osx" then t"arm64" else t"aarch64"
      case _                     => Unset

    system.let { system => architecture.let { architecture => t"$system-$architecture" } }

  def platform: Optional[Text] =
    def property(name: String): Text = Optional(java.lang.System.getProperty(name)).let(_.tt).or(t"")
    platform(property("os.name"), property("os.arch"))

  def micromambaUrl(platform: Text): Text =
    t"https://github.com/mamba-org/micromamba-releases/releases/download/$micromambaVersion/micromamba-$platform"

  // Whether a file is a recording rather than an alignment: by its extension, or by the magic
  // at its start — RIFF (WAV), fLaC, ID3 or a sync word (MP3), OggS, FORM (AIFF), or `ftyp` at
  // the fourth byte (M4A/AAC).
  private val audioExtensions: sci.Set[Text] =
    sci.Set(t"wav", t"wave", t"flac", t"mp3", t"m4a", t"aac", t"aiff", t"aif", t"ogg", t"opus", t"wma")

  // Whether the file at `path` is a recording, by its first bytes.
  def isAudio(path: Text): Boolean =
    val head: Data =
      try
        val stream: java.io.InputStream = jnf.Files.newInputStream(jnf.Path.of(path.s)).nn
        try Array.unsafeFrozen(stream.readNBytes(12).nn) finally stream.close()
      catch case _: Exception => Array.empty[Byte]

    isAudio(path, head)

  def isAudio(path: Text, head: Data): Boolean =
    val extension: Text = path.cut(t".").reverse.head.or(t"").lower
    def ascii(offset: Int, text: Text): Boolean =
      head.length >= offset + text.length
      && List.range(0, text.length).all { i => head.readUnchecked(offset + i) == text.s.charAt(i).toByte }

    audioExtensions.contains(extension)
    || ascii(0, t"RIFF") || ascii(0, t"fLaC") || ascii(0, t"ID3") || ascii(0, t"OggS")
    || ascii(0, t"FORM") || ascii(4, t"ftyp")

  // The key an alignment is cached by: the recording's bytes, the transcript, and the model.
  def key(audio: Data, transcript: Text): Text =
    val sound: Text = audio.digest[Sha2[256]].serialize[Hex]
    val words: Text = t"$transcript\n$model\n$mfaSpec".digest[Sha2[256]].serialize[Hex]
    t"$sound $words".digest[Sha2[256]].serialize[Hex].keep(24)

  // The commands, as the arguments of each process, so that they can be read and tested.
  def createCommand(home: Home): List[Text] =
    List(home.micromamba, t"create", t"-y", t"-p", home.env, t"-c", t"conda-forge", mfaSpec)

  def downloadCommand(home: Home, kind: Text): List[Text] =
    List(home.micromamba, t"run", t"-p", home.env, t"mfa", t"model", t"download", kind, model)

  def alignCommand(home: Home, corpus: Text, output: Text, wide: Boolean): List[Text] =
    val beams: List[Text] = if wide then List(t"--beam", t"100", t"--retry_beam", t"400") else Nil
    List(home.micromamba, t"run", t"-p", home.env, t"mfa", t"align", t"--clean", t"--single_speaker",
         t"--no_use_postgres", t"--output_format", t"json", t"-j", t"2")
    + beams + List(corpus, model, model, output)

  // The variables MFA and micromamba are given, over the environment's own.
  def variables(home: Home): sci.Map[Text, Text] =
    sci.Map(t"MAMBA_ROOT_PREFIX" -> home.mamba, t"MFA_ROOT_DIR" -> home.root, t"MAMBA_NO_BANNER" -> t"1")

  private def exists(path: Text): Boolean = jnf.Files.exists(jnf.Path.of(path.s))

  // Bytes written whole to, and read whole from, a file by its absolute path.
  private def write(path: jnf.Path, data: Data): Unit =
    unsafely(path.toString.tt.as[Path on Linux].write(data))

  private def read(path: jnf.Path): Optional[Data] =
    safely(path.toString.tt.as[Path on Linux].read[Data])

  // A process run to completion: its exit status and its output, both streams, the errors
  // drained apart so that neither fills while the other is read.
  private def run(arguments: List[Text], home: Home)(using Environment): (Int, Text) =
    val builder: java.lang.ProcessBuilder =
      java.lang.ProcessBuilder(arguments.map(_.s).stdlib*)

    val environment: java.util.Map[String, String] = builder.environment().nn
    variables(home).foreach { (name, value) => environment.put(name.s, value.s) }
    summon[Environment].variable(t"HOME").let { home => environment.put("HOME", home.s) }
    builder.redirectErrorStream(true)

    try
      val process: java.lang.Process = builder.start().nn
      val output: Text = String(process.getInputStream.nn.readAllBytes().nn, "UTF-8").nn.tt
      (process.waitFor(), output)
    catch case error: java.io.IOException => (-1, error.getMessage.nn.tt)

  private def tail(output: Text, lines: Int = 12): Text =
    output.cut(t"\n").filter(_.trim != t"").reverse.keep(lines).reverse.map { line => t"  $line" }.join(t"\n")

  // The manual way, said when fuel cannot do it.
  def manual(home: Home): List[Text] =
    List
      ( t"To install Montreal Forced Aligner by hand:",
        t"  brew install micromamba    # or the executable from github.com/mamba-org/micromamba-releases",
        t"  micromamba create -p ${home.env} -c conda-forge \"$mfaSpec\"",
        t"  MFA_ROOT_DIR=${home.root} micromamba run -p ${home.env} mfa model download acoustic $model",
        t"  MFA_ROOT_DIR=${home.root} micromamba run -p ${home.env} mfa model download dictionary $model" )

  // MFA, installed where it is not: micromamba fetched, the environment made, the models
  // downloaded; each step skipped when done before, and said as it happens, since each is
  // slow. `Unset` when MFA is ready; else what went wrong.
  def ensure(home: Home)(using Stdio, Environment): Optional[Text] =
    def step(name: Text)(block: => Optional[Text]): Optional[Text] = block.let { error => t"$name: $error" }

    val binary: Optional[Text] = step(t"fetching micromamba"):
      if exists(home.micromamba) then Unset else platform match
        case platform: Text =>
          Out.println(t"fuel: fetching micromamba $micromambaVersion for $platform")
          val url: Text = micromambaUrl(platform)

          safely(url.as[HttpUrl].fetch().receive[Data]).lay(t"$url could not be downloaded"):
            (data: Data) =>
              val path: jnf.Path = jnf.Path.of(home.micromamba.s).nn
              jnf.Files.createDirectories(path.getParent.nn)
              write(path, data)
              jnf.Files.setPosixFilePermissions(path, jnf.attribute.PosixFilePermissions.fromString("rwxr-xr-x"))
              Unset

        case _ =>
          t"micromamba has no executable for this platform"

    val environment: Optional[Text] = binary.or:
      step(t"installing Montreal Forced Aligner"):
        if exists(home.mfa) then Unset else
          Out.println(t"fuel: installing Montreal Forced Aligner into ${home.env} (once; about 2 GB, a few minutes)")
          run(createCommand(home), home) match
            case (0, _)      => Unset
            case (_, output) => t"micromamba could not create the environment:\n${tail(output)}"

    // An acoustic model is kept zipped, a dictionary as its text.
    def modelFile(kind: Text): Text =
      val suffix: Text = if kind == t"dictionary" then t"dict" else t"zip"
      t"${home.root}/pretrained_models/$kind/$model.$suffix"

    environment.or:
      List(t"acoustic", t"dictionary").bind: (kind: Text) =>
        if exists(modelFile(kind)) then Nil else
          Out.println(t"fuel: downloading the $model $kind model")
          run(downloadCommand(home, kind), home) match
            case (0, _)      => Nil
            case (_, output) => List(t"downloading the $kind model: \n${tail(output)}")
      . head

  enum Outcome:
    case Aligned(json: Text, cached: Boolean)
    case Failed(message: Text)

  // The recording aligned against the transcript, from the cache when it has been before: a
  // corpus of the recording beside the transcript's text, aligned by MFA to JSON, which is
  // kept by the key and read as any alignment is. An alignment MFA cannot find at its usual
  // beam is tried once more with wider ones, since a whole talk is one long utterance.
  def align(home: Home, audio: Text, transcript: Text, cache: Text)(using Stdio, Environment)
  :   Outcome =

    val source: jnf.Path = jnf.Path.of(audio.s).nn

    read(source).lay(Outcome.Failed(t"$audio could not be read")):
      (bytes: Data) =>
        val digest: Text = key(bytes, transcript)
        val kept: jnf.Path = jnf.Path.of(t"$cache/align/$digest.json".s).nn

        if jnf.Files.exists(kept) then
          Outcome.Aligned(jnf.Files.readString(kept).nn.tt, true)
        else
          val extension: Text = audio.cut(t".").reverse.head.or(t"wav")
          val work: jnf.Path = jnf.Path.of(t"$cache/align/$digest".s).nn
          val corpus: jnf.Path = work.resolve("corpus").nn
          val output: jnf.Path = work.resolve("aligned").nn
          jnf.Files.createDirectories(corpus)
          write(corpus.resolve(t"talk.$extension".s).nn, bytes)
          jnf.Files.writeString(corpus.resolve("talk.txt").nn, transcript.s)

          def attempt(wide: Boolean): (Int, Text) =
            run(alignCommand(home, corpus.toString.tt, output.toString.tt, wide), home)

          val name: Text = audio.cut(t"/").reverse.head.or(audio)
          Out.println(t"fuel: aligning $name to the transcript with Montreal Forced Aligner")
          val produced: jnf.Path = output.resolve("talk.json").nn

          val result: (Int, Text) = attempt(false) match
            case (0, text) if jnf.Files.exists(produced) => (0, text)
            case (status, text) =>
              Out.println(t"fuel: no alignment at the usual beam; trying wider beams")
              attempt(true)

          result match
            case (0, _) if jnf.Files.exists(produced) =>
              val json: Text = jnf.Files.readString(produced).nn.tt
              jnf.Files.writeString(kept, json.s)
              remove(work)
              Outcome.Aligned(json, false)

            case (_, text) =>
              Outcome.Failed(t"Montreal Forced Aligner could not align $name:\n${tail(text)}")

  private def remove(path: jnf.Path): Unit =
    if jnf.Files.isDirectory(path) then
      val children = jnf.Files.list(path).nn
      try children.forEach(child => remove(child.nn)) finally children.close()
    jnf.Files.deleteIfExists(path)
