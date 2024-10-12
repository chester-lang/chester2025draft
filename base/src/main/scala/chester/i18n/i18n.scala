package chester.i18n

import scala.io.Source
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import upickle.default._

case class Language(tag: LanguageTag, region: RegionTag) {
  def name: String = s"${tag.name}_${region.name}"
  override def toString: String = name
}

object Language {
  def apply(tag: LanguageTag): Language = new Language(tag, tag.defaultRegion)

  private val languages = LanguageTag.values
  private val regions = RegionTag.values

  def fromOption(x: String): Option[Language] = {
    val parts = x.split("[_\\-]")
    if (parts.length == 1) {
      languages.find(_.is(parts(0))).map(Language(_))
    } else if (parts.length == 2) {
      for {
        l <- languages.find(_.is(parts(0)))
        r <- regions.find(_.is(parts(1)))
      } yield Language(l, r)
    } else {
      None
    }
  }

  def from(x: String): Language = fromOption(x).getOrElse(
    throw new IllegalArgumentException(s"Invalid language $x")
  )
}

enum LanguageTag {
  case EN, ZH

  def name: String = toString.toLowerCase()
  def is(x: String): Boolean = x.toLowerCase() == name
  def defaultRegion: RegionTag = this match {
    case EN => RegionTag.NZ
    case ZH => RegionTag.TW
  }
}

enum RegionTag {
  case NZ, AU, TW, HK, US, BG

  def name: String = toString.toUpperCase()
  def is(x: String): Boolean = x.toUpperCase() == name
}

case class RegionTable(table: Map[RegionTag, Map[String, String]]) {
  private val alternatives: Vector[Map[String, String]] =
    table.toSeq.sortBy((_, map) => -map.size).map(_._2).toVector

  def get(region: RegionTag, context: String): String = {
    table.get(region).flatMap(_.get(context)) match {
      case Some(value) => value
      case None =>
        alternatives.flatMap(_.get(context)).headOption.getOrElse(context)
    }
  }

  def update(region: RegionTag, key: String, value: String): RegionTable = {
    val updatedRegionMap = table.getOrElse(region, Map()) + (key -> value)
    copy(table = table + (region -> updatedRegionMap))
  }
}

object TranslationTable {
  private var table: Map[LanguageTag, RegionTable] = loadTranslations()

  private def loadTranslations(): Map[LanguageTag, RegionTable] = {
    LanguageTag.values.map { langTag =>
      val filePath = s"translations/${langTag.name}.json"
      val langMap = if (Files.exists(Paths.get(filePath))) {
        val source = Source.fromFile(filePath)(scala.io.Codec.UTF8)
        val fileContent = try source.mkString finally source.close()
        val jsonMap = read[Map[String, Map[String, String]]](fileContent)
        val regionMap = jsonMap.map {
          case (regionName, translations) =>
            RegionTag.valueOf(regionName.toUpperCase()) -> translations
        }
        RegionTable(regionMap)
      } else {
        RegionTable(Map())
      }
      langTag -> langMap
    }.toMap
  }

  def get(lang: Language, key: String): String = {
    val translation = table.get(lang.tag).map(_.get(lang.region, key)).getOrElse(key)
    if (translation == key) {
      // Missing translation, update the translation file
      addMissingTranslation(lang, key)
    }
    translation
  }

  private def addMissingTranslation(lang: Language, key: String): Unit = {
    synchronized {
      val regionTable = table.getOrElse(lang.tag, RegionTable(Map()))
      val updatedRegionTable = regionTable.update(lang.region, key, key)
      table = table + (lang.tag -> updatedRegionTable)
      // Save back to the JSON file
      val filePath = s"translations/${lang.tag.name}.json"
      val jsonMap = updatedRegionTable.table.map {
        case (region, translations) => region.name -> translations
      }
      val jsonString = write(jsonMap, indent = 2)
      Files.write(Paths.get(filePath), jsonString.getBytes(StandardCharsets.UTF_8))
    }
  }
}

object Template {
  def stringContextToString(sc: StringContext): String = {
    val stringbuilder = new StringBuilder()
    val partsIterator = sc.parts.iterator
    var count = 1
    while (partsIterator.hasNext) {
      stringbuilder.append(partsIterator.next().replace("$", "$$"))
      if (partsIterator.hasNext) {
        stringbuilder.append(s"$$$${count}")
        count += 1
      }
    }
    stringbuilder.result()
  }

  def applyTemplate(template: String, args: Vector[Any]): String = {
    if (args.length > 9)
      throw new IllegalArgumentException("Too many arguments")
    var result = template
    val xs = args.map(_.toString)
    for (i <- xs.indices) {
      val placeholder = s"$$${i + 1}"
      result = result.replace(placeholder, xs(i))
    }
    // Check for missing arguments
    val missingArgs = "\\$\\$\\{\\d+\\}".r.findAllIn(result)
    if (missingArgs.nonEmpty) {
      throw new IllegalArgumentException(
        s"Missing arguments ${missingArgs.mkString(", ")} in template $template"
      )
    }
    result.replace("$$", "$")
  }
}
