structure(list(method = "GET", url = "geo/ows?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A4326&typeName=MarineRegions%3Aeez&cql_filter=mrgid%3D%27cannotcast%27&outputFormat=SHAPE-ZIP", 
    status_code = 400L, headers = structure(list(`Access-Control-Allow-Origin` = "*", 
        `X-Frame-Options` = "SAMEORIGIN", `Content-Encoding` = "gzip", 
        `Content-Type` = "application/xml", `Transfer-Encoding` = "chunked", 
        Date = "Wed, 28 Jun 2023 12:56:06 GMT"), class = "httr2_headers"), 
    body = charToRaw("<?xml version=\"1.0\" encoding=\"UTF-8\"?><ows:ExceptionReport xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" xmlns:ows=\"http://www.opengis.net/ows/1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/ows/1.1 geo/schemas/ows/1.1.0/owsAll.xsd\">\n<ows:Exception exceptionCode=\"NoApplicableCode\">\n<ows:ExceptionText>java.lang.ClassCastException: Cannot cast java.lang.String to java.lang.Integer\nCannot cast java.lang.String to java.lang.Integer</ows:ExceptionText>\n</ows:Exception>\n</ows:ExceptionReport>\n")), class = "httr2_response")
