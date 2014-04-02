InterVA4
========
working version of InterVA4 R replicate package

This is a working repo for the R package InterVA4. For stable version (currently version 1.1, corresponding with InterVA-4.2) of the package please visit http://cran.r-project.org/web/packages/InterVA4/index.html


If you have any questions regarding this package, or have concerns/find bugs/have new ideas in better implementations to share, please contact Zehang _(Richard)_ Li at lizehang@uw.edu. Thanks!

update 2014-04-02
-----------------
The new version 1.3 has added new features to functions:

1. Fixed non-replicate version of InterVA, stop dropping off smaller
probabilities.
2. Add new features to Population.summary, now CSMF could be calculated
with either top K causes or InterVA top 3 causes.

update 2014-03-08
-----------------
Make the code runnable as long as the order of symptom is correct. 
And output warnings for different column names.

update 2014-02-10
-----------------
The new version 1.2 has added new features to functions:

1. enable group code of COD in the output file.

2. add _noplot_ option to _Population.summary_.
