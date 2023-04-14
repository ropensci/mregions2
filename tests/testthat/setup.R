library(httptest2)

httptest2::set_redactor(function (x) {
  require(magrittr, quietly=TRUE)
  # Gazetteer
  httptest2::gsub_response(x, "https://marineregions.org/", "api/") %>%
    httptest2::gsub_response("getGazetteerRecordsByName.json", "byname/") %>%
    httptest2::gsub_response("getGazetteerRecordsByNames.json", "bynames/") %>%
    httptest2::gsub_response("getGazetteerRecordByMRGID.json", "bymrgid/") %>%
    httptest2::gsub_response("getGazetteerRecordsByLatLong.json", "bylatlong/") %>%
    httptest2::gsub_response("getGazetteerGeometries.ttl", "geom/") %>%
    httptest2::gsub_response("getGazetteerGeometry.ttl", "1geom/") %>%
    httptest2::gsub_response("getGazetteerRecordsByType.json", "bytype/") %>%
    httptest2::gsub_response("getGazetteerTypes.json", "types/") %>%
    httptest2::gsub_response("getGazetteerRecordsBySource.json", "bysource/") %>%
    httptest2::gsub_response("getGazetteerSources.json", "sources/") %>%
    httptest2::gsub_response("getGazetteerSourceBySourceID.json", "sourceid/") %>%
    httptest2::gsub_response("getGazetteerRelationsByMRGID.json", "relations/") %>%
    httptest2::gsub_response("getGazetteerWMSes.json", "wms/") %>%
    httptest2::gsub_response("getGazetteerNamesByMRGID.json", "toname/") %>%

    # Products
    httptest2::gsub_response("https://geo.vliz.be/geoserver/", "geo/")
})


