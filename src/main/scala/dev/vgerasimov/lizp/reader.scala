package dev.vgerasimov.lizp

import java.nio.file.{ Files, Path, Paths }

import scala.io.Source
import scala.util.Try

import dev.vgerasimov.lizp.syntax.*

/** Used for reading given files, nothing fancy. */
trait Reader:
  def apply(path: String)(sourcePaths: List[Path] = Nil): Either[Reader.Error, String] =
    apply(Paths.get(path))(sourcePaths)
  def apply(path: Path)(sourcePaths: List[Path]): Either[Reader.Error, String]

object Reader:
  private lazy val impl = ReaderImpl

  /** Returns implementation of [[Reader]]. */
  def apply(): Reader = impl

  /** Base file reading error. */
  sealed trait Error extends LizpError

  /** Contains [[Error]] implementations. */
  object Error:
    case class AbsolutePathNotFound(path: Path) extends Error
    case class RelativePathNotFound(path: Path, sourcePaths: List[Path]) extends Error

/** Simple [[Reader]] implementation using [[java.nio.file]] functionality. */
private object ReaderImpl extends Reader:
  override def apply(path: Path)(sourcePaths: List[Path]): Either[Reader.Error, String] =
    if (path.isAbsolute)
      Some(path)
        .filter(Files.exists(_))
        .map(Files.readString)
        .toRight(Reader.Error.AbsolutePathNotFound(path))
    else
      sourcePaths
        .map(_.resolve(path))
        .find(Files.exists(_))
        .map(Files.readString)
        .toRight(Reader.Error.RelativePathNotFound(path, sourcePaths))
