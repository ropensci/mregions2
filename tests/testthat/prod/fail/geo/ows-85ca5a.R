structure(list(method = "GET", url = "geo/ows?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A4326&typeName=MarineRegions%3Aeez&cql_filter=mrgid%20%3D%20-1&outputFormat=SHAPE-ZIP", 
    status_code = 200L, headers = structure(list(`Access-Control-Allow-Origin` = "*", 
        `X-Frame-Options` = "SAMEORIGIN", `Content-Disposition` = "attachment; filename=eez.zip", 
        `Content-Type` = "application/zip", `Transfer-Encoding` = "chunked", 
        Date = "Wed, 28 Jun 2023 12:56:16 GMT"), class = "httr2_headers"), 
    body = as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x14))), class = "httr2_response")
