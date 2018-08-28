InterVA4
========
InterVA4 R replicate package

This is an R package replicating InterVA-4 software. For stable version of the package please visit http://cran.r-project.org/web/packages/InterVA4/index.html


If you have any questions regarding this package, or have concerns/find bugs/have new ideas in better implementations to share, please contact Richard Li at lizehang@uw.edu. 

## What's new
- [CRAN version](https://cran.r-project.org/web/packages/InterVA4/news.html)
- [Current developer version (1.7.x)](InterVA4_1.7/inst/NEWS.Rd)

## Old news
#### version 1.6
- Added CSMF.interVA4() function for CSMF calculation as suggested in InterVA4 software.
- Renamed the function Population.summary() into CSMF() as to make the name consistent with CSMF.interVA4().
- Fixed problems with outputting all deaths (including those with key information missing, e.g., missing sex, age, or has no symptoms) to file.
- Added finer control over which of the two bugs in InterVA-4.02 to replicate.

#### version 1.3
- Fixed non-replicate version of InterVA, stop dropping off smaller
probabilities.
- Add new features to Population.summary, now CSMF could be calculated
with either top K causes or InterVA top 3 causes.

#### version 1.2
- Make the code runnable as long as the order of symptom is correct. 
- Output warnings for different column names.
- enable group code of COD in the output file.
- add _noplot_ option to _Population.summary_.
