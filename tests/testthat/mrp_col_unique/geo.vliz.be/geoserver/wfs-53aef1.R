structure(list(url = "https://geo.vliz.be/geoserver/wfs?service=WFS&version=2.0.0&typeName=MarineRegions:ecs&request=DescribeFeatureType",
    status_code = 200L, headers = structure(list(`access-control-allow-origin` = "*",
        `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=MarineRegions-ecs.xsd",
        `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2",
        `transfer-encoding` = "chunked", date = "Fri, 27 Jan 2023 10:18:26 GMT"), class = c("insensitive",
    "list")), all_headers = list(list(status = 200L, version = "HTTP/1.1",
        headers = structure(list(`access-control-allow-origin` = "*",
            `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=MarineRegions-ecs.xsd",
            `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2",
            `transfer-encoding` = "chunked", date = "Fri, 27 Jan 2023 10:18:26 GMT"), class = c("insensitive",
        "list")))), cookies = structure(list(domain = logical(0),
        flag = logical(0), path = logical(0), secure = logical(0),
        expiration = structure(numeric(0), class = c("POSIXct",
        "POSIXt")), name = logical(0), value = logical(0)), row.names = integer(0), class = "data.frame"),
    content = charToRaw('<?xml version="1.0" encoding="UTF-8"?><xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:MarineRegions="geo.vliz.be/MarineRegions" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:wfs="http://www.opengis.net/wfs/2.0" elementFormDefault="qualified" targetNamespace="geo.vliz.be/MarineRegions">
  <xsd:import namespace="http://www.opengis.net/gml/3.2" schemaLocation="https://geo.vliz.be/geoserver/schemas/gml/3.2.1/gml.xsd"/>
  <xsd:complexType name="ecsType">
    <xsd:complexContent>
      <xsd:extension base="gml:AbstractFeatureType">
        <xsd:sequence>
          <xsd:element maxOccurs="1" minOccurs="0" name="geoname" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="pol_type" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_ter1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_ter1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_sov1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_sov1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_ter2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_ter2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_sov2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_sov2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_ter3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_ter3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_sov3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_sov3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory4" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter4" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_ter4" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_ter4" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign4" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov4" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_sov4" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_sov4" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory5" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter5" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_ter5" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_ter5" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign5" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov5" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_sov5" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_sov5" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory6" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter6" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_ter6" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_ter6" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign6" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov6" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_sov6" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_sov6" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory7" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter7" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_ter7" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_ter7" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign7" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov7" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="iso_sov7" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="un_sov7" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="x_1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="y_1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="area_km2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid" nillable="true" type="xsd:int"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ecs" nillable="true" type="xsd:int"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="the_geom" nillable="true" type="gml:MultiSurfacePropertyType"/>
        </xsd:sequence>
      </xsd:extension>
    </xsd:complexContent>
  </xsd:complexType>
  <xsd:element name="ecs" substitutionGroup="gml:AbstractFeature" type="MarineRegions:ecsType"/>
</xsd:schema>'), date = structure(1674814706, class = c("POSIXct",
    "POSIXt"), tzone = "GMT"), times = c(redirect = 0, namelookup = 6.4e-05,
    connect = 6.5e-05, pretransfer = 0.000127, starttransfer = 0.009729,
    total = 0.010025)), class = "response")
