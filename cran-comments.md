mregions2 is a package to explore and retrieve data from MarineRegions.org. It 
superseeds mregions. The package has been peer-reviewed via rOpenSci. See 
https://github.com/ropensci/software-review/issues/590

This is resubmission after the package was archived on CRAN for not fixing
issues on time. Bad timing that this happened during the Christmas period.

## R CMD check results (local)

0 errors | 0 warnings | 0 notes

Locally on Ubuntu:
  R version 4.4.1 (2024-06-14)
  Platform: x86_64-pc-linux-gnu
  Running under: Ubuntu 22.04.4 LTS
  
## R CMD check results (Win-devel)

0 errors | 0 warnings | 2 notes

  * checking CRAN incoming feasibility ... [11s] NOTE
  Maintainer: 'Salvador Jesús Fernández Bejarano <salvador.fernandez@vliz.be>'
  
  New submission
  
  Package was archived on CRAN
  
  Possibly misspelled words in DESCRIPTION:
    Marineregions (3:25)
    geospatial (17:42)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2025-01-13 as issues were not corrected
      in time.
    
In this case, these two words are not misspelled in my opinion:

"Marineregions" references to "Marineregions.org", the url to the Marine 
Regions website at https://marineregions.org and it is a common term we use to 
refer to the project. 

"Geospatial" is an term in the English language defined e.g. in the Cambridge 
Business English Dictionary.

  * checking DESCRIPTION meta-information ... NOTE
  Author field differs from that derived from Authors@R
    Author:    'Salvador Jesús Fernández Bejarano [aut, cre] (<https://orcid.org/0000-0003-0535-7677>, salvafern), Lotte Pohl [aut] (<https://orcid.org/0000-0002-7607-7018>, lottepohl), Julia Gustavsen [rev], Muralidhar M.A. [rev], Sheila M. Saia [rev], LifeWatch Belgium [fnd] (lifewatch.be)'
    Authors@R: 'Salvador Jesús Fernández Bejarano [aut, cre] (ORCID: <https://orcid.org/0000-0003-0535-7677>, github: salvafern), Lotte Pohl [aut] (ORCID: <https://orcid.org/0000-0002-7607-7018>, github: lottepohl), Julia Gustavsen [rev], Muralidhar M.A. [rev], Sheila M. Saia [rev], LifeWatch Belgium [fnd] (lifewatch.be)'

I have searched extensively how could this be fixed. In the DESCRIPTION file, 
there is only the field Authors@R. There is no fiedls Author nor Maintainer.
This issue is not triggered in the R CMD check of Win-release.

Thank you for considering mregions2 to be hosted in CRAN,
Salva Fernández
