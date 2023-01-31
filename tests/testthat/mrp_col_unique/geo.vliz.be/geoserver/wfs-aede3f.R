structure(list(url = "https://geo.vliz.be/geoserver/wfs?service=WFS&version=2.0.0&typeName=MarineRegions:high_seas&request=DescribeFeatureType",
    status_code = 200L, headers = structure(list(`access-control-allow-origin` = "*",
        `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=MarineRegions-high_seas.xsd",
        `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2",
        `transfer-encoding` = "chunked", date = "Fri, 27 Jan 2023 10:18:27 GMT"), class = c("insensitive",
    "list")), all_headers = list(list(status = 200L, version = "HTTP/1.1",
        headers = structure(list(`access-control-allow-origin` = "*",
            `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=MarineRegions-high_seas.xsd",
            `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2",
            `transfer-encoding` = "chunked", date = "Fri, 27 Jan 2023 10:18:27 GMT"), class = c("insensitive",
        "list")))), cookies = structure(list(domain = logical(0),
        flag = logical(0), path = logical(0), secure = logical(0),
        expiration = structure(numeric(0), class = c("POSIXct",
        "POSIXt")), name = logical(0), value = logical(0)), row.names = integer(0), class = "data.frame"),
    content = charToRaw('
      <?xml version="1.0" encoding="UTF-8"?><xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:MarineRegions="geo.vliz.be/MarineRegions" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:wfs="http://www.opengis.net/wfs/2.0" elementFormDefault="qualified" targetNamespace="geo.vliz.be/MarineRegions">
        <xsd:import namespace="http://www.opengis.net/gml/3.2" schemaLocation="https://geo.vliz.be/geoserver/schemas/gml/3.2.1/gml.xsd"/>
        <xsd:complexType name="high_seasType">
          <xsd:complexContent>
            <xsd:extension base="gml:AbstractFeatureType">
              <xsd:sequence>
                <xsd:element maxOccurs="1" minOccurs="0" name="__gid" nillable="true" type="xsd:long"/>
                <xsd:element maxOccurs="1" minOccurs="0" name="mrgid" nillable="true" type="xsd:decimal"/>
                <xsd:element maxOccurs="1" minOccurs="0" name="source" nillable="true" type="xsd:string"/>
                <xsd:element maxOccurs="1" minOccurs="0" name="area_km2" nillable="true" type="xsd:decimal"/>
                <xsd:element maxOccurs="1" minOccurs="0" name="the_geom" nillable="true" type="gml:MultiSurfacePropertyType"/>
              </xsd:sequence>
            </xsd:extension>
          </xsd:complexContent>
        </xsd:complexType>
        <xsd:element name="high_seas" substitutionGroup="gml:AbstractFeature" type="MarineRegions:high_seasType"/>
      </xsd:schema>
      '), date = structure(1674814707, class = c("POSIXct",
    "POSIXt"), tzone = "GMT"), times = c(redirect = 0, namelookup = 0.000147,
    connect = 0.000151, pretransfer = 0.000312, starttransfer = 0.00682,
    total = 0.007997)), class = "response")
