structure(list(url = "geo/wfs?service=WFS&version=2.0.0&typeNames=MarineRegions:ecs&cql_filter=mrgid%20=%2064123&request=GetFeature", 
    status_code = 200L, headers = structure(list(`access-control-allow-origin` = "*", 
        `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=ecs.xml", 
        `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2", 
        `transfer-encoding` = "chunked", date = "Thu, 09 Feb 2023 09:30:29 GMT"), class = c("insensitive", 
    "list")), all_headers = list(list(status = 200L, version = "HTTP/1.1", 
        headers = structure(list(`access-control-allow-origin` = "*", 
            `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=ecs.xml", 
            `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2", 
            `transfer-encoding` = "chunked", date = "Thu, 09 Feb 2023 09:30:29 GMT"), class = c("insensitive", 
        "list")))), cookies = structure(list(domain = logical(0), 
        flag = logical(0), path = logical(0), secure = logical(0), 
        expiration = structure(numeric(0), class = c("POSIXct", 
        "POSIXt")), name = logical(0), value = logical(0)), row.names = integer(0), class = "data.frame"), 
    content = charToRaw('<?xml version="1.0" encoding="UTF-8"?>
<wfs:FeatureCollection xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:wfs="http://www.opengis.net/wfs/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:MarineRegions="geo.vliz.be/MarineRegions" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" numberMatched="1" numberReturned="1" timeStamp="2023-02-09T09:42:55.495Z" xsi:schemaLocation="http://www.opengis.net/wfs/2.0 https://geo.vliz.be/geoserver/schemas/wfs/2.0/wfs.xsd geo.vliz.be/MarineRegions https://geo.vliz.be/geoserver/wfs?service=WFS&amp;version=2.0.0&amp;request=DescribeFeatureType&amp;typeName=MarineRegions%3Aecs http://www.opengis.net/gml/3.2 https://geo.vliz.be/geoserver/schemas/gml/3.2.1/gml.xsd">
	<wfs:member>
		<MarineRegions:ecs gml:id="ecs.3">
			<MarineRegions:geoname>Mexican Extended Continental Shelf (DOALOS Deposit)</MarineRegions:geoname>
			<MarineRegions:pol_type>ECS DOALOS Deposit</MarineRegions:pol_type>
			<MarineRegions:territory1>Mexico</MarineRegions:territory1>
			<MarineRegions:mrgid_ter1>2224</MarineRegions:mrgid_ter1>
			<MarineRegions:iso_ter1>MEX</MarineRegions:iso_ter1>
			<MarineRegions:un_ter1>484</MarineRegions:un_ter1>
			<MarineRegions:sovereign1>Mexico</MarineRegions:sovereign1>
			<MarineRegions:mrgid_sov1>2224</MarineRegions:mrgid_sov1>
			<MarineRegions:iso_sov1>MEX</MarineRegions:iso_sov1>
			<MarineRegions:un_sov1>484</MarineRegions:un_sov1>
			<MarineRegions:x_1>-92.77077</MarineRegions:x_1>
			<MarineRegions:y_1>25.44703</MarineRegions:y_1>
			<MarineRegions:area_km2>10798</MarineRegions:area_km2>
			<MarineRegions:mrgid>64123</MarineRegions:mrgid>
			<MarineRegions:mrgid_ecs>64123</MarineRegions:mrgid_ecs>
			<MarineRegions:the_geom>
				<gml:MultiSurface srsName="urn:ogc:def:crs:EPSG::4326" srsDimension="2" gml:id="ecs.3.the_geom">
					<gml:surfaceMember>
						<gml:Polygon gml:id="ecs.3.the_geom.1">
							<gml:exterior>
								<gml:LinearRing>
									<gml:posList>0 1 1 1 1 0 0 0 0 1</gml:posList>								
								</gml:LinearRing>
							</gml:exterior>
						</gml:Polygon>
					</gml:surfaceMember>
				</gml:MultiSurface>
			</MarineRegions:the_geom>
		</MarineRegions:ecs>
	</wfs:member>
</wfs:FeatureCollection>
	'), date = structure(1675935029, class = c("POSIXct", 
    "POSIXt"), tzone = "GMT"), times = c(redirect = 0, namelookup = 6.2e-05, 
    connect = 6.4e-05, pretransfer = 0.00018, starttransfer = 0.023594, 
    total = 0.024219)), class = "response")