# set_redactor(function (x) {
#   gsub_response(x, "https://marineregions.org/", "api/")
# })



set_redactor(function (x) {
  require(magrittr, quietly=TRUE)
  gsub_response(x, "https://marineregions.org/", "api/") %>%
    gsub_response("getGazetteerRecordsByName.json", "byname/") %>%
    gsub_response("getGazetteerRecordsByNames.json", "bynames/") %>%
    gsub_response("getGazetteerRecordByMRGID.json", "bymrgid/") %>%
    gsub_response("getGazetteerRecordsByLatLong.json", "bylatlong/") %>%
    gsub_response("getGazetteerGeometries.ttl", "geom/") %>%
    gsub_response("getGazetteerRecordsByType.json", "bytype/") %>%
    gsub_response("getGazetteerTypes.json", "types/") %>%
    gsub_response("getGazetteerRecordsBySource.json", "bysource/") %>%
    gsub_response("getGazetteerSources.json", "sources/") %>%
    gsub_response("getGazetteerSourceBySourceID.json", "sourceid/") %>%
    gsub_response("getGazetteerRelationsByMRGID.json", "relations/") %>%
    gsub_response("getGazetteerWMSes.json", "wms/") %>%
    gsub_response("getGazetteerNamesByMRGID.json", "toname/")
})

