# InterVA4 - changes

Version 1.7.6 (2019-11-11) 
==========================
* Fix output csv file format error
* Fix error that CSMF calculation omitted deaths with 100% undetermined.

Version 1.7.5 (2018-02-19) 
==========================
* Fix error in extracting CSMF for pregnancy related death

Changes in version 1.7.4 (2017-05-26)
========================== 
* Update license to GPL-3
* Add author Peter Byass

Changes in version 1.7.3 (2016-12-08)
===========================
* Update the built-in conditional probability matrix to be consistent with the released InterVA-4.03 software.
* The option _replicate = FALSE_ now sets the implementation to be the same as InterVA-4.03, while the individual bug replicate options remain to be fixing the bugs based on InterVA-4.02.
* The updated conditional probability and data check rules for InterVA-4.03 are summarized as follows:
    * if sk_les = Yes don't ask sk_feet
    * if sk_feet = Yes don't ask sk_les
    * if ch_rash = Yes don't ask measrash
    * if measrash = Yes don't ask herpes
    * if measrash = Yes don't ask rash
    * if herpes = Yes don't ask measrash
    * if herpes = Yes don't ask rash
    * prior CSMF for HIV/AIDS change from E to C (overwritten later, not effective)
    * prior CSMF for Malaria change from E to C (overwritten later, not effective)
