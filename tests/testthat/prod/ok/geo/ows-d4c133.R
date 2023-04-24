structure(list(method = "GET", url = "geo/ows?service=wfs&version=2.0.0&request=GetFeature&typeName=MarineRegions%3Aeez&cql_filter=sovereign1%20%3D%20%27Portugal%27&outputFormat=SHAPE-ZIP", 
    status_code = 200L, headers = structure(list(`Access-Control-Allow-Origin` = "*", 
        `X-Frame-Options` = "SAMEORIGIN", `Content-Disposition` = "attachment; filename=eez.zip", 
        `Content-Type` = "application/zip", `Transfer-Encoding` = "chunked", 
        Date = "Mon, 24 Apr 2023 15:45:16 GMT"), class = "httr2_headers"), 
    body = as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x14))), class = "httr2_response")
