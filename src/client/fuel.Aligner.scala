                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                ╭────────╮                   ╭───╮                                ┃
┃                                │   ╭────╯                   │   │                                ┃
┃                                │   │                        │   │                                ┃
┃                                │   ╰──╮╭───╮ ╭───╮╭────────╮│   │                                ┃
┃                                │   ╭──╯│   │ │   ││   ╭─╮  ││   │                                ┃
┃                                │   │   │   │ │   ││   ╰─╯  ││   │                                ┃
┃                                │   │   │   │ │   ││   ╭────╯│   │                                ┃
┃                                │   │   │   ╰─╯   ││   ╰────╮│   │                                ┃
┃                                ╰───╯   ╰────╌╰───╯╰────────╯╰───╯                                ┃
┃                                                                                                  ┃
┃    Fuel, version 0.6.0.                                                                          ┃
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

import alphabets.hexLowerCase
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import filesystemBackends.javaBaseFilesystem
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import providers.javaBaseProvider
import pyrocosm.Filing
import systems.javaBaseSystem

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
    def property(name: Text): Text = summon[System](name).or(t"")
    platform(property(t"os.name"), property(t"os.arch"))

  def micromambaUrl(platform: Text): Text =
    t"https://github.com/mamba-org/micromamba-releases/releases/download/$micromambaVersion/micromamba-$platform"

  // Whether a file is a recording rather than an alignment: by its extension, or by the magic
  // at its start — RIFF (WAV), fLaC, ID3 or a sync word (MP3), OggS, FORM (AIFF), or `ftyp` at
  // the fourth byte (M4A/AAC).
  private val audioExtensions: Set[Text] =
    Set
      ( t"wav", t"wave", t"flac", t"mp3", t"m4a", t"aac", t"aiff", t"aif", t"ogg", t"opus",
        t"wma" )

  private def extension(path: Text): Text = path.cut(t".").reverse.head.or(t"").lower

  // Whether the file at `path` is a recording: by its name, or else by its first bytes, which
  // are read only then, since a recording is long.
  def isAudio(path: Text): Boolean =
    audioExtensions.has(extension(path))
    || isAudio(path, place(path).let(Filing.data(_)).or(Array.empty[Byte]))

  def isAudio(path: Text, head: Data): Boolean =
    def ascii(offset: Int, text: Text): Boolean =
      val bytes: Data = text.sysData

      head.length >= offset + bytes.length
      && List.range(0, bytes.length).all: (index: Int) =>
          head.readUnchecked(offset + index) == bytes.readUnchecked(index)

    audioExtensions.has(extension(path))
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
  def variables(home: Home): Map[Text, Text] =
    Map
      ( t"MAMBA_ROOT_PREFIX" -> home.mamba,
        t"MFA_ROOT_DIR"      -> home.root,
        t"MAMBA_NO_BANNER"   -> t"1" )

  // A file by its absolute path.
  private def place(path: Text): Optional[Path on Linux] = safely(path.as[Path on Linux])

  // A process run to completion: its exit status and its output, standard output and errors
  // interleaved as they were written, which is how micromamba and MFA are read when they fail.
  // Its variables are MFA's and micromamba's, over the JVM's own, and `HOME` the environment's.
  // A gap in Soundness: guillotine reads a process's output and its errors apart, and has no
  // way to read them interleaved (soundness#2503), so the JDK's `ProcessBuilder` is used here,
  // and here alone. A process which cannot be started has no output, and a status of -1.
  private def run(arguments: List[Text], home: Home)(using Environment): (Int, Text) =
    val home0: Optional[Text] = summon[Environment].variable(t"HOME")

    safely:
      val builder: java.lang.ProcessBuilder = java.lang.ProcessBuilder(arguments.map(_.s)*)
      val environment: java.util.Map[String, String] = builder.environment().nn
      variables(home).each { (name, value) => environment.put(name.s, value.s) }
      home0.let { home => environment.put("HOME", home.s) }
      builder.redirectErrorStream(true)
      val process: java.lang.Process = builder.start().nn
      val output: Text = String(process.getInputStream.nn.readAllBytes().nn, "UTF-8").nn.tt
      (process.waitFor(), output)

    . or((-1, t"${arguments.head.or(t"the command")} could not be started"))

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
      if onDisk(home.micromamba) then Unset else platform match
        case platform: Text =>
          Out.println(t"fuel: fetching micromamba $micromambaVersion for $platform")
          val url: Text = micromambaUrl(platform)

          safely(url.as[HttpUrl].fetch().receive[Data]).lay(t"$url could not be downloaded"):
            (data: Data) =>
              val installed: Boolean = place(home.micromamba).lay(false): (path: Path on Linux) =>
                Filing.replace(path, data) && safely(path.executable() = true).present

              if installed then Unset else t"${home.micromamba} could not be written"

        case _ =>
          t"micromamba has no executable for this platform"

    val environment: Optional[Text] = binary.or:
      step(t"installing Montreal Forced Aligner"):
        if onDisk(home.mfa) then Unset else
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
        if onDisk(modelFile(kind)) then Nil else
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

    place(audio).let(Filing.data(_)).lay(Outcome.Failed(t"$audio could not be read")):
      (bytes: Data) =>
        val digest: Text = key(bytes, transcript)
        val kept: Optional[Path on Linux] = place(t"$cache/align/$digest.json")

        kept.let(Filing.text(_)).lay(fresh(home, audio, transcript, cache, digest, bytes)):
          (json: Text) => Outcome.Aligned(json, true)

  // An alignment made afresh, in a working directory beside where it is kept, which is
  // removed once the alignment is.
  private def fresh
    ( home: Home, audio: Text, transcript: Text, cache: Text, digest: Text, bytes: Data )
    ( using Stdio, Environment )
  :   Outcome =

    val suffix: Text = audio.cut(t".").reverse.head.or(t"wav")
    val work: Text = t"$cache/align/$digest"
    val corpus: Text = t"$work/corpus"
    val output: Text = t"$work/aligned"
    place(t"$corpus/talk.$suffix").let(Filing.replace(_, bytes))
    place(t"$corpus/talk.txt").let(Filing.replace(_, transcript))

    def attempt(wide: Boolean): (Int, Text) =
      run(alignCommand(home, corpus, output, wide), home)

    val name: Text = audio.cut(t"/").reverse.head.or(audio)
    Out.println(t"fuel: aligning $name to the transcript with Montreal Forced Aligner")
    val produced: Optional[Path on Linux] = place(t"$output/talk.json")
    def made: Boolean = produced.lay(false)(Filing.exists(_))

    val result: (Int, Text) = attempt(false) match
      case (0, text) if made => (0, text)
      case (status, text) =>
        Out.println(t"fuel: no alignment at the usual beam; trying wider beams")
        attempt(true)

    result match
      case (0, _) if made =>
        val json: Text = produced.let(Filing.text(_)).or(t"")
        place(t"$cache/align/$digest.json").let(Filing.replace(_, json))
        place(work).let(Filing.wipe(_))
        Outcome.Aligned(json, false)

      case (_, text) =>
        Outcome.Failed(t"Montreal Forced Aligner could not align $name:\n${tail(text)}")
