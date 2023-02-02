# Get Turtle doc and replace geometry with dummy polygons: POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))
# curl https://marineregions.org/rest/getGazetteerGeometries.ttl/58/ > 58.txt
structure(list(method = "GET", url = "api/rest/geom//58/", status_code = 200L, 
    headers = structure(list(date = "Thu, 02 Feb 2023 14:28:55 GMT", 
        server = "Apache/2.4.53 (Win64)", `content-security-policy` = "upgrade-insecure-requests; script-src * 'unsafe-inline' 'unsafe-eval' blob:; object-src *; frame-ancestors 'self' www.vliz.be vliz.be;", 
        `access-control-allow-origin` = "*", `access-control-allow-headers` = "X-Requested-With, Content-Type, Accept, Origin, Authorization", 
        `access-control-allow-methods` = "GET, POST, OPTIONS", 
        `content-length` = "32041", `content-type` = "text/turtle; charset=UTF-8;", 
        `set-cookie` = "vliz_webc=vliz_webc2; path=/"), class = "httr2_headers"), 
    body = charToRaw('
		@prefix mr: <http://marineregions.org/ns/ontology#> .
		@prefix gsp: <http://www.opengis.net/ont/geosparql#> .
		@prefix prov: <http://www.w3.org/ns/prov#> .
		@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
		@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

		<http://marineregions.org/mrgid/58> mr:hasGeometry <http://marineregions.org/mrgid/58/geometries?source=3&attributeValue=WD16>, <http://marineregions.org/mrgid/58/geometries?source=182&attributeValue=34023> .
		<http://marineregions.org/mrgid/58/geometries?source=3&attributeValue=WD16>
		  gsp:asWKT "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))"^^gsp:wktLiteral ;
		  prov:hadPrimarySource <http://www.vliz.be/projects/sail> .

		<http://www.vliz.be/projects/sail> rdfs:label "SAIL"^^xsd:string .
		<http://marineregions.org/mrgid/58/geometries?source=182&attributeValue=34023>
		  gsp:asWKT "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))"^^gsp:wktLiteral ;
		  prov:hadPrimarySource [ prov:wasAttributedTo [ rdfs:label "Flanders Marine Institute (VLIZ)"^^xsd:string ] ] .
		')), class = "httr2_response")