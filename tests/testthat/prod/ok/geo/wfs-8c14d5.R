structure(list(url = "geo/wfs?service=WFS&version=2.0.0&typeNames=MarineRegions:ecs_boundaries&cql_filter=line_id%20=%204232&request=GetFeature", 
    status_code = 200L, headers = structure(list(`access-control-allow-origin` = "*", 
        `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=ecs_boundaries.xml", 
        `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2", 
        `transfer-encoding` = "chunked", date = "Thu, 09 Feb 2023 09:55:07 GMT"), class = c("insensitive", 
    "list")), all_headers = list(list(status = 200L, version = "HTTP/1.1", 
        headers = structure(list(`access-control-allow-origin` = "*", 
            `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=ecs_boundaries.xml", 
            `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2", 
            `transfer-encoding` = "chunked", date = "Thu, 09 Feb 2023 09:55:07 GMT"), class = c("insensitive", 
        "list")))), cookies = structure(list(domain = logical(0), 
        flag = logical(0), path = logical(0), secure = logical(0), 
        expiration = structure(numeric(0), class = c("POSIXct", 
        "POSIXt")), name = logical(0), value = logical(0)), row.names = integer(0), class = "data.frame"), 
    content = charToRaw('<?xml version="1.0" encoding="UTF-8"?>
<wfs:FeatureCollection xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:wfs="http://www.opengis.net/wfs/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:MarineRegions="geo.vliz.be/MarineRegions" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" numberMatched="1" numberReturned="1" timeStamp="2023-02-09T09:55:32.909Z" xsi:schemaLocation="http://www.opengis.net/wfs/2.0 https://geo.vliz.be/geoserver/schemas/wfs/2.0/wfs.xsd geo.vliz.be/MarineRegions https://geo.vliz.be/geoserver/wfs?service=WFS&amp;version=2.0.0&amp;request=DescribeFeatureType&amp;typeName=MarineRegions%3Aecs_boundaries http://www.opengis.net/gml/3.2 https://geo.vliz.be/geoserver/schemas/gml/3.2.1/gml.xsd">
	<wfs:member>
		<MarineRegions:ecs_boundaries gml:id="ecs_boundaries.310">
			<MarineRegions:line_id>4232.0</MarineRegions:line_id>
			<MarineRegions:line_name>Mexico ECS CLCS Submission</MarineRegions:line_name>
			<MarineRegions:line_type>ECS CLCS Submission</MarineRegions:line_type>
			<MarineRegions:territory1>Mexico</MarineRegions:territory1>
			<MarineRegions:sovereign1>Mexico</MarineRegions:sovereign1>
			<MarineRegions:origin>Database</MarineRegions:origin>
			<MarineRegions:url1>http://marineregions.org/documents/01_Executive_Summary.pdf</MarineRegions:url1>
			<MarineRegions:source1>EXECUTIVE SUMMARY A PARTIAL SUBMISSION OF DATA AND INFORMATION ON THE OUTER LIMITS OF THE CONTINENTAL SHELF OF THE UNITED MEXICAN STATES PURSUANT TO PART VI OF AND ANNEX II TO THE UNITED NATIONS CONVENTION ON THE LAW OF THE SEA</MarineRegions:source1>
			<MarineRegions:doc_date>2011-12-19</MarineRegions:doc_date>
			<MarineRegions:mrgid_ter1>2224.0</MarineRegions:mrgid_ter1>
			<MarineRegions:mrgid_sov1>2224.0</MarineRegions:mrgid_sov1>
			<MarineRegions:length_km>98.06</MarineRegions:length_km>
			<MarineRegions:the_geom>
				<gml:MultiCurve srsName="urn:ogc:def:crs:EPSG::4326" srsDimension="2" gml:id="ecs_boundaries.310.the_geom">
					<gml:curveMember>
						<gml:LineString gml:id="ecs_boundaries.310.the_geom.1">
							<gml:posList>0 0 1 1</gml:posList>
						</gml:LineString>
					</gml:curveMember>
				</gml:MultiCurve>
			</MarineRegions:the_geom>
		</MarineRegions:ecs_boundaries>
	</wfs:member>
</wfs:FeatureCollection>'), date = structure(1675936507, class = c("POSIXct", 
    "POSIXt"), tzone = "GMT"), times = c(redirect = 0, namelookup = 0.00012, 
    connect = 0.000122, pretransfer = 0.000339, starttransfer = 0.042369, 
    total = 0.042848)), class = "response")
