structure(list(url = "https://geo.vliz.be/geoserver/wfs?service=WFS&version=2.0.0&typeNames=MarineRegions:eez_24nm&cql_filter=mrgid%20=%2049243&request=GetFeature",
               status_code = 200L, headers = structure(list(`access-control-allow-origin` = "*",
                                                            `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=eez_24nm.xml",
                                                            `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2",
                                                            `transfer-encoding` = "chunked", date = "Fri, 27 Jan 2023 14:27:58 GMT"), class = c("insensitive",
                                                                                                                                                "list")), all_headers = list(list(status = 200L, version = "HTTP/1.1",
                                                                                                                                                                                  headers = structure(list(`access-control-allow-origin` = "*",
                                                                                                                                                                                                           `x-frame-options` = "SAMEORIGIN", `content-disposition` = "inline; filename=eez_24nm.xml",
                                                                                                                                                                                                           `content-encoding` = "gzip", `content-type` = "application/gml+xml; version=3.2",
                                                                                                                                                                                                           `transfer-encoding` = "chunked", date = "Fri, 27 Jan 2023 14:27:58 GMT"), class = c("insensitive",
                                                                                                                                                                                                                                                                                               "list")))), cookies = structure(list(domain = logical(0),
                                                                                                                                                                                                                                                                                                                                    flag = logical(0), path = logical(0), secure = logical(0),
                                                                                                                                                                                                                                                                                                                                    expiration = structure(numeric(0), class = c("POSIXct",
                                                                                                                                                                                                                                                                                                                                                                                 "POSIXt")), name = logical(0), value = logical(0)), row.names = integer(0), class = "data.frame"),
               content = charToRaw('<?xml version="1.0" encoding="UTF-8"?>
                          <wfs:FeatureCollection xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:wfs="http://www.opengis.net/wfs/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:MarineRegions="geo.vliz.be/MarineRegions" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" numberMatched="1" numberReturned="1" timeStamp="2023-01-27T13:50:01.059Z" xsi:schemaLocation="http://www.opengis.net/wfs/2.0 https://geo.vliz.be/geoserver/schemas/wfs/2.0/wfs.xsd geo.vliz.be/MarineRegions https://geo.vliz.be/geoserver/wfs?service=WFS&amp;version=2.0.0&amp;request=DescribeFeatureType&amp;typeName=MarineRegions%3Aeez_24nm http://www.opengis.net/gml/3.2 https://geo.vliz.be/geoserver/schemas/gml/3.2.1/gml.xsd">
                          	<wfs:member>
                          		<MarineRegions:eez_24nm gml:id="eez_24nm.1">
                          			<MarineRegions:mrgid>49243</MarineRegions:mrgid>
                          			<MarineRegions:geoname>Belgian 24 NM</MarineRegions:geoname>
                          			<MarineRegions:pol_type>24NM</MarineRegions:pol_type>
                          			<MarineRegions:mrgid_ter1>14</MarineRegions:mrgid_ter1>
                          			<MarineRegions:territory1>Belgium</MarineRegions:territory1>
                          			<MarineRegions:mrgid_sov1>14</MarineRegions:mrgid_sov1>
                          			<MarineRegions:sovereign1>Belgium</MarineRegions:sovereign1>
                          			<MarineRegions:iso_ter1>BEL</MarineRegions:iso_ter1>
                          			<MarineRegions:x_1>2.65877</MarineRegions:x_1>
                          			<MarineRegions:y_1>51.48562</MarineRegions:y_1>
                          			<MarineRegions:mrgid_eez>3293</MarineRegions:mrgid_eez>
                          			<MarineRegions:area_km2>1161</MarineRegions:area_km2>
                          			<MarineRegions:iso_sov1>BEL</MarineRegions:iso_sov1>
                          			<MarineRegions:un_sov1>56.0</MarineRegions:un_sov1>
                          			<MarineRegions:un_ter1>56.0</MarineRegions:un_ter1>
                          			<MarineRegions:the_geom>
                          				<gml:MultiSurface srsName="urn:ogc:def:crs:EPSG::4326" srsDimension="2" gml:id="eez_24nm.1.the_geom">
                          					<gml:surfaceMember>
                          						<gml:Polygon gml:id="eez_24nm.1.the_geom.1">
                          							<gml:exterior>
                          								<gml:LinearRing>
                          									<gml:posList>0 1 1 1 1 0 0 0 0 1</gml:posList>
                          								</gml:LinearRing>
                          							</gml:exterior>
                          						</gml:Polygon>
                          					</gml:surfaceMember>
                          				</gml:MultiSurface>
                          			</MarineRegions:the_geom>
                          		</MarineRegions:eez_24nm>
                          	</wfs:member>
                          </wfs:FeatureCollection>
                        '), date = structure(1674829678, class = c("POSIXct",
                                                                   "POSIXt"), tzone = "GMT"), times = c(redirect = 0, namelookup = 5.4e-05,
                                                                                                        connect = 5.5e-05, pretransfer = 0.000112, starttransfer = 0.04286,
                                                                                                        total = 0.043484)), class = "response")
