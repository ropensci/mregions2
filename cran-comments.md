mregions2 is a package to explore and retrieve data from MarineRegions.org. It 
superseeds mregions. The package has been peer-reviewed via rOpenSci. See 
https://github.com/ropensci/software-review/issues/590

This is resubmission after correcting a number of issues:

> Please reduce the length of the title to less than 65 characters.

Done

>Please provide a link to the used webservices (Marineregions.org) to the
description field of your DESCRIPTION file in the form
<http:...> or <[https:...]https:...>
with angle brackets for auto-linking and no space after 'http:' and
'https:'.

Done


>\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user. Does not seem necessary.
Please replace \dontrun with \donttest.
Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}.
Please put functions which download data in \donttest{}.

Done. I wrapped most functions under \donttest{} as they retrieve data from an 
API. Cannot be sure the examples won't take longer than 5 seconds always.


>Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir(). -> man/mrp_get.Rd

Changed the examples on man/mrp_get.Rd to write to `tempdir()`

## R CMD check results (local)

0 errors | 0 warnings | 0 notes

Locally on Ubuntu:
  R version 4.4.1 (2024-06-14)
  Platform: x86_64-pc-linux-gnu
  Running under: Ubuntu 22.04.4 LTS
  
## R CMD check results (Win-devel)

0 errors | 0 warnings | 1 note

  * checking CRAN incoming feasibility ... [11s] NOTE
  Maintainer: 'Salvador Jesús Fernández Bejarano <salvador.fernandez@vliz.be>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Marineregions (3:25)
    geospatial (19:42)
    
In this case, these two words are not misspelled in my opinion:

"Marineregions" references to "Marineregions.org", the url to the Marine 
Regions website at https://marineregions.org and it is a common term we use to 
refer to the project. 

"Geospatial" is an term in the English language defined e.g. in the Cambridge 
Business English Dictionary.

Thank you for considering mregions2 to be hosted in CRAN,
Salva Fernández
