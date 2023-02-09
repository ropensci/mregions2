httptest::set_redactor(function (x) {
  require(magrittr, quietly=TRUE)
  gsub_response(x, "https://geo.vliz.be/geoserver/", "geo/")
})

httptest::set_requester(function (request) {
  gsub_request(request, "https://geo.vliz.be/geoserver/", "geo/")
})
