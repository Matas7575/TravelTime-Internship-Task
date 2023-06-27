import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LocationMatcherTest extends AnyFlatSpec with Matchers {
  "ReadJSON" should "return an empty list of matched results when no locations and regions are provided" in {
    val locations = List.empty[Location]
    val regions = List.empty[Region]

    val matchedResults = LocationMatcher.matchLocationsToRegions(locations, regions)

    matchedResults shouldBe empty
  }

  it should "return an empty list of matched results when locations and regions are provided, but there are no matches" in {
    val locations = List(
      Location("location1", List(23.889176780105828,
        55.65496979450708)),
      Location("location2", List(23.45504886775234,
        55.80041318671479))
    )
    val regions = List(
      Region(
        "region1",
        List(
          List(
            List(23.40559125748419,
              55.67976438661299),
            List(23.40559125748419,
              55.54319960872607),
            List(23.67486046894379,
              55.54319960872607),
            List(23.67486046894379,
              55.67976438661299),
            List(23.40559125748419,
              55.67976438661299),
          )
        )
      )
    )

    val matchedResults = LocationMatcher.matchLocationsToRegions(locations, regions)

    matchedResults shouldBe empty
  }


  it should "match locations to regions" in {
    val locations = List(
      Location("location1", List(23.294976544712625, 55.54418279256754)),
      Location("location2", List(24.003868958555074, 55.366565866519664)),
      Location("location3", List(23.60271278638035, 55.28528180001621)),
      Location("location4", List(23.943420768226844, 55.1630429190738)),
      Location("location5", List(25.5370500799838, 55.6528504115299))
    )

    val regions = List(
      Region(
        "region1",
        List(
          List(
            List(22.877334502448576, 55.66524976150424),
            List(22.877334502448576, 54.860544651769715),
            List(24.4709686110873, 54.860544651769715),
            List(24.4709686110873, 55.66524976150424),
            List(22.877334502448576, 55.66524976150424)
          )
        )
      ),
      Region(
        "region2",
        List(
          List(
            List(24.61933664500873, 55.85694744029246),
            List(24.61933664500873, 55.39778518102966),
            List(25.63047001048949, 55.39778518102966),
            List(25.63047001048949, 55.85694744029246),
            List(24.61933664500873, 55.85694744029246)
          )
        )
      )
    )

    val expectedMatches = List(
      MatchedResult("region1", List("location1", "location2", "location3", "location4")),
      MatchedResult("region2", List("location5")),
    )

    val matchedResults = LocationMatcher.matchLocationsToRegions(locations, regions)
    matchedResults should contain theSameElementsAs expectedMatches
  }
  it should "handle overlapping regions" in {
    val locations = List(
      Location("location1", List(23.652137639456754,
        55.58252309526503)),
      Location("location2", List(23.87744453067745,
        55.69727846239371)),
      Location("location3", List(24.05329381163142,
        55.526574544815844)),
      Location("location4", List(24.003836201363214,
        55.454968762584656)),
      Location("location5", List(24.278600702852145,
        55.355126666635414)),
        Location("location6", List(24.278600702852145,
          55.355126666635414))
    )

    val regions = List(
      Region(
        "region1",
        List(
          List(
            List(23.360887267878354,
              55.790076734189995),
            List(23.360887267878354,
              55.38635442094804),
            List(24.19067606237587,
              55.38635442094804),
            List(24.19067606237587,
              55.790076734189995),
            List(23.360887267878354,
              55.790076734189995)
          )
        )
      ),
      Region(
        "region2",
        List(
          List(
            List(24.520393464163647,
              55.28320926825478),
            List(24.520393464163647,
              55.6166746747289),
            List(23.904920980826915,
              55.6166746747289),
            List(23.904920980826915,
              55.28320926825478),
            List(24.520393464163647,
              55.28320926825478)
          )
        )
      )
    )

    val expectedMatches = List(
      MatchedResult("region1", List("location1", "location2", "location3", "location4")),
      MatchedResult("region2", List("location3","location4","location5","location6")),
    )

    val matchedResults = LocationMatcher.matchLocationsToRegions(locations, regions)
    matchedResults should contain theSameElementsAs expectedMatches
  }


}
