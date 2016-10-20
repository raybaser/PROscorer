#' PROscorer
#'
#' @description An open-source repository of functions to score specific
#'   Patient-Reported Outcome (PRO), Quality of Life (QoL), and other
#'   psychometric measures commonly used in research.
#'
#' @details The \pkg{PROscorer} package contains functions to score specific
#'   PRO, QoL, and other psychometric measures and questionnaire-based
#'   instruments commonly used in research.  Additionally, \pkg{PROscorer} is
#'   accompanied by a package vignette that contains detailed descriptions of
#'   each instrument scored by \pkg{PROscorer}, complete with references.
#'   Importantly, the instrument summaries are written according to a set of
#'   standards that ensure they meet "best practice" guidelines for descriptions
#'   of PRO-like measures in formal research protocols and in reports of
#'   research results featuring such measures. This means that, with little or
#'   no editing, a given instrument summary can be copied and pasted directly
#'   into protocols, grant proposals, and manuscripts.  In addition to improving
#'   the measure descriptions in research documents, this saves the study
#'   investigators considerable time and effort.
#'
#'
#' @section The Problem:
#'   The scientific rigor and reproducibility of research involving PRO, QoL,
#'   and similar measures is lagging behind other research areas.  Two primary
#'   reasons for these shortcomings are (1) measurement error introduced by
#'   faulty scoring procedures, and (2) inadequate, incomplete, and/or
#'   inaccurate descriptions of PRO-like measures in research protocols and in
#'   published results of studies that incorporate such measures.
#'
#'   Scoring procedures represent a major source of error in research
#'   studies that rely upon PRO and similar measures.  These errors typically go
#'   unnoticed, hidden, and/or ignored, eroding the scientific integrity of the
#'   research and hindering progress in the numerous scientific fields that
#'   conduct studies that use these measures.  A seemingly minor scoring error can
#'   appreciably compromise measurement validity and reliability, as well as make
#'   research results impossible to reproduce and replicate.
#'
#'   Inadequate, incomplete, and/or inaccurate descriptions of PRO-like measures
#'   in research documents can negatively impact research quality at multiple
#'   stages in the research process.  During initial study planning and
#'   protocol/proposal writing, inadequate descriptions of PRO-like measures can
#'   lead investigators to make uninformed choices and to select outcome
#'   measures that are relatively poor study endpoints for addressing the
#'   research questions.  Poor descriptions of PRO-like measures can also cause
#'   difficulty during critical scientific review and institutional evaluation
#'   of research protocols and proposals, delaying or preventing the approval of
#'   studies. When the study data are collected and ready to be analyzed, poor
#'   descriptions can cause confusion about the study endpoints for the
#'   statistician(s), who often rely directly upon research protocols to
#'   understand the outcome measures and to guide their data manipulation
#'   (including scoring) and analysis.  Finally, poor descriptions of PRO-like
#'   measures in published study results hinder interpretation of the results by
#'   readers, and bring into question whether the results can be validly
#'   compared to other studies that purportedly utilized the same outcome
#'   measure.
#'
#' @section The Proposed Solution:
#'   The \pkg{PROscorer} and \pkg{PROscorerTools} packages are intended to help
#'   address these problems with research involving PRO-like measures. The lofty
#'   goal of the \pkg{PROscorer} package is to eliminate these serious
#'   deficiencies in PRO-based research by serving as the gold-standard open-source
#'   repository of scoring syntax and instrument descriptions for PRO-like
#'   measures commonly used in research and clinical settings.
#'
#'   The roadmap for accomplishing this goal is as follows:
#'   \enumerate{
#'     \item Establish a small set of well-tested, reliable helper functions (in
#'       the \pkg{PROscorerTools} package) to serve as the primary building
#'       blocks for the scoring functions in the \pkg{PROscorer} package.  This
#'       makes it easy to write new scoring functions to add to \pkg{PROscorer},
#'       and decreases the chance of errors and other bugs in the
#'       \pkg{PROscorer} scoring functions.
#'     \item Elicit feedback from end-users of the \pkg{PROscorer} functions on
#'       their experiences using the functions, including suggestions for
#'       improving their usability as well as reports of any bugs or other
#'       unexpected behavior they encounter.
#'     \item Encourage researchers, statisticians, psychometricians, and other
#'       users to use the \pkg{PROscorerTools} package to write new scoring
#'       functions for their favorite PRO-like measures, and to submit them for
#'       inclusion in \pkg{PROscorer}.  Submitted functions will be carefully
#'       reviewed and tested in collaboration with the function author to ensure
#'       accurate and bug-free scoring before being included in a future version
#'       of \pkg{PROscorer}.  The \pkg{PROscorerTools} and \pkg{PROscorer}
#'       packages will include a series of vignettes teaching users how to write
#'       new functions using the \pkg{PROscorerTools} building blocks, how to
#'       submit their functions for potential inclusion in \pkg{PROscorer}, and
#'       how to write the corresponding instrument descriptions with all of the
#'       required detail. [These vignettes are currently incomplete or not yet
#'       written.  They will be added in their entirety to upcoming versions of
#'       the packages.]
#'     \item Spread the word about the \pkg{PROscorer} and \pkg{PROscorerTools}
#'       packages through social media, blogs, and asking users to recognize and
#'       cite the \pkg{PROscorer} package in research protocols and manuscripts
#'       that use the \pkg{PROscorer} scoring functions and/or the corresponding
#'       instrument descriptions.
#'     }
#'
#'
#' @section Use with Caution!:
#'   The \pkg{PROscorer} and \pkg{PROscorerTools} packages are still in their
#'   initial versions.  As such, some details and other conventions are still
#'   being hammered out (e.g., function naming conventions, standard function
#'   arguments, etc.).  These might change in future versions.  Most, if not
#'   all, of the changes to the \pkg{PROscorer} functions are expected to have
#'   little visible impact on end-users.  However, more noticeable,
#'   backwards-incompatible changes might be made to the underlying
#'   \pkg{PROscorerTools} functions before stabilizing within the next few
#'   versions of the package.  Your feedback, collaboration on GitHub, and
#'   patience during this process is welcomed and greatly appreciated.
#'
#'
#' @name PROscorer
#'
#' @docType package
NULL
