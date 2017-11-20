% Check from R:
%  news(db = tools:::.build_news_db_from_package_NEWS_Rd("~/R/Pkgs/Matrix/inst/NEWS.Rd"))
\name{NEWS}
\title{News for \R Package \pkg{InterVA4}}
\encoding{UTF-8}

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}
\section{Changes in version 1.7.4 (2017-05-26)}{
 \itemize{ 
    \item Update license to GPL-3
    \item Add author Peter Byass
    }
}
\section{Changes in version 1.7.3 (2016-12-08)}{
  \itemize{ 
    \item Update the built-in conditional probability matrix to be consistent with the released InterVA-4.03 software.
    \item The option \code{replicate = FALSE} now sets the implementation to be the same as InterVA-4.03, while the individual bug replicate options remain to be fixing the bugs based on InterVA-4.02.
    \item The updated conditional probability and data check rules for InterVA-4.03 are summarized as follows:
      \itemize{
        \item if sk_les = Yes don't ask sk_feet
        \item if sk_feet = Yes don't ask sk_les
        \item if ch_rash = Yes don't ask measrash
        \item if measrash = Yes don't ask herpes
        \item if measrash = Yes don't ask rash
        \item if herpes = Yes don't ask measrash
        \item if herpes = Yes don't ask rash
        \item prior CSMF for HIV/AIDS change from E to C (overwritten later, not effective)
        \item prior CSMF for Malaria change from E to C (overwritten later, not effective)
      }
    }
}
