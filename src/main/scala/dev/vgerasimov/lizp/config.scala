package dev.vgerasimov.lizp

import java.nio.file.{ Path, Paths }

import dev.vgerasimov.lizp.build.BuildInfo
import dev.vgerasimov.lizp.syntax.{ *, given }

case class LizpConfig(
  sourcePaths: Seq[Path] = Seq(),
  file: Path = null,
  verbosity: LizpConfig.Verbosity = LizpConfig.Verbosity.Default
)

object LizpConfig:
  enum Verbosity:
    case Default
    case Notes
  def verbosity(string: String): Option[Verbosity] = string.toUpperCase match
    case "DEFAULT" => Some(Verbosity.Default)
    case "NOTES"   => Some(Verbosity.Notes)
    case _         => None

trait ArgsParser extends (Array[String] => Either[ArgsParser.Error, LizpConfig])

object ArgsParser:
  private lazy val impl = ScoptArgsParser

  def apply(): ArgsParser = impl

  case class Error(message: String) extends LizpError

private object ScoptArgsParser extends ArgsParser:
  import scopt.{ DefaultOEffectSetup, OEffect, OParser, Read }

  private given Read[Path] = Read.reads(Paths.get(_))

  enum MsgType:
    case Info
    case Warn
    case Err

  override def apply(args: Array[String]): Either[ArgsParser.Error, LizpConfig] =
    val builder = OParser.builder[LizpConfig]
    val parser1 = {
      import builder.*
      OParser.sequence(
        programName(BuildInfo.name),
        head(BuildInfo.name, BuildInfo.version),
        help('h', "help")
          .text("Prints this message"),
        opt[Seq[Path]]('I', "include")
          .action((x, c) => c.copy(sourcePaths = x))
          .text("Paths to include"),
        opt[String]("verbosity")
          .optional()
          .validate(s => {
            val options = Set("notes", "default")
            if (options.contains(s)) success else failure(s"Option --verbosity must be one of $options")
          })
          .action((x, c) => c.copy(verbosity = LizpConfig.verbosity(x).get)),
        arg[Path]("<file>")
          .unbounded()
          .action((x, c) => c.copy(file = x))
          .text("Script to run")
      )
    }

    OParser.runParser(parser1, args, LizpConfig()) match
      case (result, effects) =>
        result match
          case Some(config) => config.asRight
          case _ =>
            ArgsParser
              .Error(
                effects
                  .map({
                    case OEffect.DisplayToOut(msg)      => (MsgType.Info, msg)
                    case OEffect.DisplayToErr(msg)      => (MsgType.Err, msg)
                    case OEffect.ReportError(msg)       => (MsgType.Err, msg)
                    case OEffect.ReportWarning(msg)     => (MsgType.Warn, msg)
                    case OEffect.Terminate(Left(msg))   => (MsgType.Err, msg)
                    case OEffect.Terminate(Right(unit)) => (MsgType.Info, "")
                  })
                  .toString
              )
              .asLeft
