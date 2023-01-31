structure(list(url = "https://geo.vliz.be/geoserver/wfs?service=WFS&version=2.0.0&typeName=MarineRegions:eez_boundaries&request=DescribeFeatureType",
    status_code = 200L, headers = structure(list(`access-control-allow-origin` = "*",
        `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=MarineRegions-eez_boundaries.xsd",
        `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2",
        `transfer-encoding` = "chunked", date = "Fri, 27 Jan 2023 10:18:26 GMT"), class = c("insensitive",
    "list")), all_headers = list(list(status = 200L, version = "HTTP/1.1",
        headers = structure(list(`access-control-allow-origin` = "*",
            `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=MarineRegions-eez_boundaries.xsd",
            `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2",
            `transfer-encoding` = "chunked", date = "Fri, 27 Jan 2023 10:18:26 GMT"), class = c("insensitive",
        "list")))), cookies = structure(list(domain = logical(0),
        flag = logical(0), path = logical(0), secure = logical(0),
        expiration = structure(numeric(0), class = c("POSIXct",
        "POSIXt")), name = logical(0), value = logical(0)), row.names = integer(0), class = "data.frame"),
    content = charToRaw('<?xml version="1.0" encoding="UTF-8"?><xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:MarineRegions="geo.vliz.be/MarineRegions" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:wfs="http://www.opengis.net/wfs/2.0" elementFormDefault="qualified" targetNamespace="geo.vliz.be/MarineRegions">
  <xsd:import namespace="http://www.opengis.net/gml/3.2" schemaLocation="https://geo.vliz.be/geoserver/schemas/gml/3.2.1/gml.xsd"/>
  <xsd:complexType name="eez_boundariesType">
    <xsd:complexContent>
      <xsd:extension base="gml:AbstractFeatureType">
        <xsd:sequence>
          <xsd:element maxOccurs="1" minOccurs="0" name="line_id" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="line_name" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="line_type" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov1" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter1" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_ter2" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="territory2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_sov2" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="sovereign2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_eez1" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="eez1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_eez2" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="eez2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="source1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="url1" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="source2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="url2" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="source3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="url3" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="origin" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="doc_date" nillable="true" type="xsd:date"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="mrgid_jreg" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="joint_reg" nillable="true" type="xsd:string"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="length_km" nillable="true" type="xsd:decimal"/>
          <xsd:element maxOccurs="1" minOccurs="0" name="the_geom" nillable="true" type="gml:MultiCurvePropertyType"/>
        </xsd:sequence>
      </xsd:extension>
    </xsd:complexContent>
  </xsd:complexType>
  <xsd:element name="eez_boundaries" substitutionGroup="gml:AbstractFeature" type="MarineRegions:eez_boundariesType"/>
</xsd:schema>'), date = structure(1674814706, class = c("POSIXct",
    "POSIXt"), tzone = "GMT"), times = c(redirect = 0, namelookup = 8.6e-05,
    connect = 8.8e-05, pretransfer = 2e-04, starttransfer = 0.007725,
    total = 0.007964)), class = "response")
