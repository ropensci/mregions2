# Get Turtle doc and replace geometry with dummy polygons: POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))
# curl https://marineregions.org/rest/getGazetteerGeometries.ttl/4280/ > 4280.txt
structure(list(method = "GET", url = "api/rest/geom//4280/", 
    status_code = 200L, headers = structure(list(date = "Thu, 02 Feb 2023 14:28:10 GMT", 
        server = "Apache/2.4.53 (Win64)", `content-security-policy` = "upgrade-insecure-requests; script-src * 'unsafe-inline' 'unsafe-eval' blob:; object-src *; frame-ancestors 'self' www.vliz.be vliz.be;", 
        `access-control-allow-origin` = "*", `access-control-allow-headers` = "X-Requested-With, Content-Type, Accept, Origin, Authorization", 
        `access-control-allow-methods` = "GET, POST, OPTIONS", 
        `content-length` = "9454254", `content-type` = "text/turtle; charset=UTF-8;", 
        `set-cookie` = "vliz_webc=vliz_webc2; path=/"), class = "httr2_headers"), 
    body = charToRaw('
		@prefix mr: <http://marineregions.org/ns/ontology#> .
		@prefix gsp: <http://www.opengis.net/ont/geosparql#> .
		@prefix prov: <http://www.w3.org/ns/prov#> .
		@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
		@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

		<http://marineregions.org/mrgid/4280> mr:hasGeometry <http://marineregions.org/mrgid/4280/geometries?source=25&attributeValue=28B>, <http://marineregions.org/mrgid/4280/geometries?source=25&attributeValue=28f>, <http://marineregions.org/mrgid/4280/geometries?source=25&attributeValue=28g>, <http://marineregions.org/mrgid/4280/geometries?source=25&attributeValue=28h> .
		<http://marineregions.org/mrgid/4280/geometries?source=25&attributeValue=28B>
		  gsp:asWKT "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))"^^gsp:wktLiteral ;
		  prov:hadPrimarySource [ prov:wasAttributedTo [ rdfs:label "(1953). Limits of oceans and seas. 3rd edition. IHO Special Publication, 23. International Hydrographic Organization (IHO): Monaco. 38 pp."^^xsd:string ] ] .

		<http://marineregions.org/mrgid/4280/geometries?source=25&attributeValue=28f>
		  gsp:asWKT "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))"^^gsp:wktLiteral ;
		  prov:hadPrimarySource [ prov:wasAttributedTo [ rdfs:label "(1953). Limits of oceans and seas. 3rd edition. IHO Special Publication, 23. International Hydrographic Organization (IHO): Monaco. 38 pp."^^xsd:string ] ] .

		<http://marineregions.org/mrgid/4280/geometries?source=25&attributeValue=28g>
		  gsp:asWKT "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))"^^gsp:wktLiteral ;
		  prov:hadPrimarySource [ prov:wasAttributedTo [ rdfs:label "(1953). Limits of oceans and seas. 3rd edition. IHO Special Publication, 23. International Hydrographic Organization (IHO): Monaco. 38 pp."^^xsd:string ] ] .

		<http://marineregions.org/mrgid/4280/geometries?source=25&attributeValue=28h>
		  gsp:asWKT "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))"^^gsp:wktLiteral ;
		  prov:hadPrimarySource [ prov:wasAttributedTo [ rdfs:label "(1953). Limits of oceans and seas. 3rd edition. IHO Special Publication, 23. International Hydrographic Organization (IHO): Monaco. 38 pp."^^xsd:string ] ] .
	')), class = "httr2_response")