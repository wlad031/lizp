package dev.vgerasimov.lizp

import java.nio.file.{ Files, Path, Paths }

import scala.util.Try

import dev.vgerasimov.lizp.syntax.*

sealed trait Source:
  def tryRead(toReadPath: Path): Option[String]

object Source:

  def resource(name: String): Source.Resource = new Source.Resource(name)
  def file(path: Path): Source.File = new Source.File(path)
  def directory(path: Path): Source.Directory = new Source.Directory(path)
  def directory(path: String): Source.Directory = directory(Paths.get(path))
  def apply(path: Path): Source = if (path.toFile.isFile) file(path) else directory(path)

  class Resource private[Source] (name: String) extends Source:
    import java.io.FileOutputStream

    private lazy val content = new String(classOf[Reader].getResourceAsStream(name).readAllBytes())

    override def tryRead(toReadPath: Path): Option[String] =
      if (toReadPath.normalize.equals(Paths.get(name))) Some(content)
      else None

    override val toString: String = s"Resource($name)"

  class File private[Source] (path: Path) extends Source:
    require(path.toFile.isFile, s"Cannot create file from $path: not file")
    require(path.isAbsolute, s"Cannot create file from $path: not absolute")

    private lazy val content = ???

    override def tryRead(toReadPath: Path): Option[String] = ???

  class Directory private[Source] (path: Path) extends Source:
    require(path.toFile.isDirectory, s"Cannot create directory from $path: not directory")

    override def tryRead(toReadPath: Path): Option[String] =
      val resolved = path.resolve(toReadPath)
      if (!Files.exists(resolved)) None
      else Some(Files.readString(resolved))

    override val toString: String = s"Directory($path)"

type SourcePath = Set[Source]

/** Used for reading given files, nothing fancy. */
trait Reader:
  def apply(path: String)(sourcePath: SourcePath): Either[Reader.Error, String] =
    apply(Paths.get(path))(sourcePath)
  def apply(path: Path)(sourcePath: SourcePath): Either[Reader.Error, String]

object Reader:
  private lazy val impl = ReaderImpl

  /** Returns implementation of [[Reader]]. */
  def apply(): Reader = impl

  /** Base file reading error. */
  sealed trait Error extends LizpError

  /** Contains [[Error]] implementations. */
  object Error:
    case class AbsolutePathNotFound(path: Path) extends Error
    case class RelativePathNotFound(path: Path, sourcePath: SourcePath) extends Error

/** Simple [[Reader]] implementation using [[java.nio.file]] functionality. */
private object ReaderImpl extends Reader:
  override def apply(path: Path)(sourcePath: SourcePath): Either[Reader.Error, String] =
    if (path.isAbsolute)
      Some(path)
        .filter(Files.exists(_))
        .map(Files.readString)
        .toRight(Reader.Error.AbsolutePathNotFound(path))
    else
      sourcePath
        .map(_.tryRead(path))
        .find(_.isDefined)
        .map(_.get)
        .toRight(Reader.Error.RelativePathNotFound(path, sourcePath))
