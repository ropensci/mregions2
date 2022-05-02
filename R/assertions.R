
methods <- c("getGazetteerRecordByMRGID", "getGazetteerGeometry" , "getGazetteerTypes", "getGazetteerGeometries", "getGazetteerRecordsByName", "getGazetteerRecordsByType", "getGazetteerWMSes", "getGazetteerRecordsByLatLong", "getGazetteerRecordsByNames", "getGazetteerSources", "getGazetteerNamesByMRGID", "getGazetteerRecordsBySource", "getFeed", "getGazetteerRelationsByMRGID")
api_type <- "rest"
file_format <- "json"
file_format <- 2
method <- "getGazetteerRecordByMRGIDxxx"
x = "a"
api_type = "restx"
api_type = 2
coll = makeAssertCollection()

print(coll$isEmpty())
assertNumeric(x, add = coll)
coll$isEmpty()
coll$push("please change x.")
coll$getMessages()
if (FALSE) {
  reportAssertions(coll)
}


# AssertCollection for the methods ####
method_coll <- makeAssertCollection()
method_coll$push("\U02139 Check `https://marineregions.org/gazetteer.php?p=webservices&type=rest` for available methods.")
method_coll$getMessages()


# assert() ####
# assert() combines multiple assertions but only outputs the first error
checkmate::assert(checkmate::assert_choice(api_type, c("rest", "soap")), checkmate::assert_choice(method, methods), combine = "and")


api_type %>% checkmate::check_choice(c("rest", "soap"))
method %>% checkmate::check_choice(methods)
