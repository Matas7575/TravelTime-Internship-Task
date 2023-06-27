import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.write

import java.nio.file.{Files, Paths}

case class Location(name: String, coordinates: List[Double])
case class Region(name: String, coordinates: List[List[List[Double]]])
case class MatchedResult(region: String, matchedLocations: List[String])

object LocationMatcher {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def main(args: Array[String]): Unit = {
    val workingDirectory = Paths.get("src/main/scala").toAbsolutePath.toString
    val locationsJson = scala.io.Source.fromFile(s"$workingDirectory/locations.json").mkString
    val regionsJson = scala.io.Source.fromFile(s"$workingDirectory/regions.json").mkString

    val locations = parse(locationsJson).extract[List[Location]]
    val regions = parse(regionsJson).extract[List[Region]]

    val matchedLocations = matchLocationsToRegions(locations, regions)

    val matchedResultsJson = write(matchedLocations)
    println(matchedResultsJson)

    // Write the matched results to a separate file
    val workingDirectory2 = Paths.get("src/main/scala").toAbsolutePath.toString
    val outputFile = s"$workingDirectory2/matched_results.json"
    Files.write(Paths.get(outputFile), matchedResultsJson.getBytes)
    println(s"Matched results written to $outputFile")
  }

  def matchLocationsToRegions(locations: List[Location], regions: List[Region]): List[MatchedResult] = {
    regions.flatMap { region =>
      val matchedLocationNames = locations
        .filter(location => isLocationInsideRegion(location, region))
        .map(_.name)

      //println(s"Region: ${region.name}, Matched Locations: $matchedLocationNames")

      if (matchedLocationNames.nonEmpty) Some(MatchedResult(region.name, matchedLocationNames))
      else None
    }
  }


  def isLocationInsideRegion(location: Location, region: Region): Boolean = {
    region.coordinates.exists(containsPoint(_, location.coordinates))
  }

  def containsPoint(polygon: List[List[Double]], point: List[Double]): Boolean = {
    val n = polygon.length
    var inside = false
    var i = 0
    var j = n - 1

    while (i < n) {
      if (((polygon(i)(1) > point(1)) != (polygon(j)(1) > point(1))) &&
        (point(0) < (polygon(j)(0) - polygon(i)(0)) * (point(1) - polygon(i)(1)) /
          (polygon(j)(1) - polygon(i)(1)) + polygon(i)(0))) {
        inside = !inside
      }
      j = i
      i += 1
    }

    inside
  }
}
