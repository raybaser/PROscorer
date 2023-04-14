#' @title Score the EORTC QLQ-BLM30 Quality of Life Questionnaire
#'
#' @description Scores the European Organization for Research and Treatment of
#'   Cancer (EORTC) QLQ-BLM30 Muscle-Invasive Bladder Cancer Module.
#'
#' @param df A data frame containing responses to the 30 QLQ-BLM30 items, and
#'   possibly other variables.
#' @param items A character vector with the QLQ-BLM30 item names, or a numeric
#'   vector indicating the column numbers of the QLQ-BLM30 items in \code{df}.
#'   If \code{items} is omitted, then \code{qlq_blm30} will assume that
#'   \code{df} contains \strong{ONLY} the QLQ-BLM30 items and no other variables.
#'   See Details for more information.
#' @param keepNvalid Logical, whether to return variables containing the
#'   number of valid, non-missing items on each scale for each respondent should
#'   be returned in the data frame with the scale scores.  The default is
#'   \code{FALSE}.  Set to \code{TRUE} to return these variables, which will be
#'   named \code{"scalename_N"} (e.g., \code{QL_N}).  Most users should omit
#'   this argument entirely.  This argument might be removed from future
#'   versions of the package, so please let me know if you think this argument
#'   useful and would rather it remain a part of the function.
#'
#'
#' @details
#' This function returns a total of 7 different scores from the EORTC
#' QLQ-BLM30. Scores are calculated according to the official scoring algorithms
#' from the EORTC.
#'
#' In addition to the name of your data frame containing the QLQ-BLM30 item
#' responses (\code{df}), you need to tell the function how to find the
#' variables that correspond to the QLQ-BLM30 items in \code{df}.  You can do this
#' in 1 of 2 ways:
#'   \enumerate{
#'     \item The first way is to manually provide the item names or locations
#'       using the \code{items} argument.  For example, if your first 10
#'       variables in \code{df} contain demographics, followed by the 30 QLQ-BLM30
#'       items \strong{in order} starting with the 11th variable, then you could
#'       use \code{items = 11:40}.
#'     \item The second way only applies if your data frame (\code{df}) contains
#'       \strong{ONLY} the 30 variables corresponding to the 30 QLQ-BLM30 items,
#'       in order, with no other non-QLQ-BLM30 variables.  In this case, you can
#'       just use the \code{df} argument and omit \code{items}.
#'  }
#'
#'
#'
#' @section How Missing Data is Handled:
#'   For all scale scores **except for the Sexual Functioning scale**, the
#'   \code{qlq_blm30} function will calculate the scale scores as long as at
#'   least half of the items on the given scale have valid, non-missing item
#'   responses.  Scores calculated in the presence of missing items are
#'   pro-rated so that their theoretical minimum and maximum values are
#'   identical to those from scores calculated from complete data.
#'
#'   The Sexual Functioning scale is handled differently because it has several
#'   conditional questions that may not be applicable to a given patient (i.e.,
#'   some questions apply only to men or only to women, and some questions apply
#'   only to sexually active respondents).  As a result, the Sexual Functioning
#'   scale may have 2, 4, 6, or 7 questions applicable for a given respondent.
#'   Because the minimum number of applicable items is 2, the \code{qlq_blm30}
#'   function will calculate the  Sexual Functioning scale score as long as at
#'   least 1 of the items has a valid, non-missing response.  Note that this can
#'   result in the \code{qlq_blm30} function scoring the Sexual Functioning
#'   scale for some respondents when they should technically be assigned a
#'   missing value for the scale.
#'
#'
#' @return
#' A data frame with all 7 of the QLQ-BLM30 scores is returned.  Of the 7
#' scores, there are 5 multi-item Symptom Scales, 1 single-item Symptom Scale
#' (Catheter Use Problems), and 1 multi-item Functional Scale (Sexual
#' Functioning) (see below).  All scores are scaled to range from 0-100, even
#' scores based on single items.  Be aware that these single-item scales still
#' have only 4 possible values, even though they are transformed to range from
#' 0-100.  The scale names and numbers of items are listed below.
#'
#' \strong{Symptom Scales (higher is more symptoms, worse functioning)}
#' \itemize{
#'   \item \strong{BLM_US} - Urinary Symptoms (from 7 items)
#'   \item \strong{BLM_UP} - Urostomy Problems (from 6 items)
#'   \item \strong{BLM_FU} - Future Perspective (from 3 items)
#'   \item \strong{BLM_BAF} - Abdominal Bloating and Flatulence (from 2 item)
#'   \item \strong{BLM_BI} - Body Image (from 3 item)
#'   \item \strong{BLM_CU} - Catheter Use Problems (from 1 item)
#' }
#'
#' \strong{Functional Scales (higher is better functioning)}
#' \itemize{
#'   \item \strong{BLM_SX} - Sexual Functioning (from 2, 4, 6, or 7 items,
#'   depending on the respondent)
#' }
#'
#' Optionally, the data frame can additionally have variables containing the
#' number of valid item responses on each scale for each respondent (if
#' \code{keepNvalid = TRUE}, but this option might be removed in future package
#' updates).
#'
#' @references
#'
#' The QLQ-BLM30 validation study does not appear to be publicly available.
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dat <- PROscorerTools::makeFakeData(n = 10, nitems = 30, prefix = "blm", values = 1:4)
#' qlq_blm30(dat, items = 1:30)
#' }


qlq_blm30 <- function(df, items = NULL, keepNvalid = FALSE) {

  # Check arguments that are unique to qlq_blm30,
  # or that require additional checks not already done by scoreScale().


  #  Get dfItems, a df with only the items -------------------------------------
  dfItems <- PROscorerTools::get_dfItems(df, items )

  # Check that dfItems has 30 items
  if (!PROscorerTools::chk_nitems(dfItems, 30)) {
    stop(paste(strwrap(
      "The QLQ-BLM30 has 30 items, but the function found a different number
      of items given the arguments and data you supplied.",
      exdent = 2, width = 65), collapse = "\n"))
  }

  ## Check item ranges:
  ## Not strictly necessary since scoreScale() will check, too.
  ## However, the scoreScale() error msgs would be confusing.
  ## Best to check ranges and give helpful error msgs for specific questionnaires.

  if (!PROscorerTools::chk_values(dfItems = dfItems[1:30],
                                  values = c(1:4, NA))) {
    stop(paste(strwrap(
      "At least one of your items has a value that is not allowed.
      Items should have all integer values between 1 and 4,
      or NA (i.e., missing).",
      exdent = 2, width = 65), collapse = "\n"))
  }




  BLM_US <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 1:7,
                                       revitems = FALSE,
                                       scalename ="BLM_US" )

  BLM_UP <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 8:13,
                                       revitems = FALSE,
                                       scalename ="BLM_UP" )

  BLM_FU <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 15:17,
                                       revitems = FALSE,
                                       scalename ="BLM_FU" )

  BLM_BAF <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 18:19,
                                       revitems = FALSE,
                                       scalename ="BLM_BAF" )

  BLM_BI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 20:22,
                                       revitems = FALSE,
                                       scalename ="BLM_BI" )

  BLM_CU <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 14,
                                       revitems = FALSE,
                                       scalename ="BLM_CU" )

  BLM_SX <- PROscorerTools::scoreScale( df = dfItems, okmiss = .80,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 23:30,
                                        revitems = c(25:28, 30),
                                        scalename ="BLM_SX" )



  scoreDF <- data.frame(BLM_US, BLM_UP, BLM_FU, BLM_BAF, BLM_BI, BLM_CU, BLM_SX)

  return(scoreDF)
}


