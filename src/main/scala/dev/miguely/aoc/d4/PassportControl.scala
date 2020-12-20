package dev.miguely.aoc.d4

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object PassportControl extends App {
  val passports = new ArrayBuffer[HashMap[String, String]]()
  var passportData = new HashMap[String, String]()
  val required = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val validationFunctions = Seq[HashMap[String, String] => Boolean](
    validateYear("byr", 1920, 2002),
    validateYear("iyr", 2010, 2020),
    validateYear("eyr", 2020, 2030),
    validateHeight(_),
    validateHairColor(_),
    validateEyeColor(_),
    validatePid(_)
  )

  passports += passportData;
  scala.io.Source
    .fromFile("input/d4.txt")
    .getLines()
    .foreach(line => {
      var trimmedLine = line.trim()
      if (trimmedLine.size == 0) {
        passportData = new HashMap[String, String]()
        passports += passportData
      } else {
        trimmedLine
          .split(" ")
          .map(data => {
            var keyValue = data.trim().split(":");
            passportData += keyValue(0) -> keyValue(1)
          })

      }
    })
  var count = 0
  for (passport <- passports) {
    val status = validatePassportAdvanced(passport)

    if (status) count = count + 1
  }
  println(count)

  // Implementation part 1
  def validatePassportSimple(passport: HashMap[String, String]): Boolean = {
    return required.foldLeft(true)((status, field) =>
      status && passport.contains(field)
    )
  }
  //implementation part 2
  def validatePassportAdvanced(passport: HashMap[String, String]): Boolean = {
    for (func <- validationFunctions) {
      if (!func(passport)) return false
    }
    return true
  }

  def validateYear(field: String, min: Int, max: Int)(
      passport: HashMap[String, String]
  ): Boolean = {
    passport.getOrElse(field, "").toIntOption match {
      case None                                        => return false
      case Some(value) if (value < min || value > max) => return false
      case _                                           => return true
    }
  }

  def validateHeight(passport: HashMap[String, String]): Boolean = {
    val value = passport.getOrElse("hgt", "")
    val cmPattern = "^([0-9]+)cm$".r
    val inPattern = "^([0-9]+)in$".r
    cmPattern.findFirstMatchIn(value) match {
      case Some(m) =>
        val cm = m.group(1).toIntOption.get
        return (cm >= 150 && cm <= 193)
      case None =>
        inPattern.findFirstMatchIn(value) match {
          case Some(m) =>
            val in = m.group(1).toIntOption.get
            return (in >= 59 && in <= 76)
          case None => return false
        }
    }
  }

  def validateHairColor(passport: HashMap[String, String]): Boolean = {
    val value = passport.getOrElse("hcl", "")
    val colorPattern = "^#([0-9a-f]{6})$".r
    colorPattern.findFirstMatchIn(value) match {
      case Some(m) =>
        return true
      case None =>
        return false
    }
  }
  def validatePid(passport: HashMap[String, String]): Boolean = {
    val value = passport.getOrElse("pid", "")
    val colorPattern = "^([0-9]{9})$".r
    colorPattern.findFirstMatchIn(value) match {
      case Some(m) =>
        return true
      case None =>
        return false
    }
  }

  def validateEyeColor(passport: HashMap[String, String]): Boolean = {
    val value = passport.getOrElse("ecl", "")
    val validEyeColor = Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    var valid = false
    validEyeColor.foreach(color => { valid = valid || color == value })
    return valid
  }
}
