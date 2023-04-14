#' @title Score the EORTC QLQ-CX24 Quality of Life Questionnaire
#'
#' @description Scores the European Organization for Research and Treatment of
#'   Cancer (EORTC) QLQ-CX24 Cervical Cancer Module.
#'
#' @param df A data frame containing responses to the 24 QLQ-CX24 items, and
#'   possibly other variables.
#' @param items A character vector with the QLQ-CX24 item names, or a numeric
#'   vector indicating the column numbers of the QLQ-CX24 items in \code{df}.
#'   If \code{items} is omitted, then \code{qlq_cx24} will assume that
#'   \code{df} contains \strong{ONLY} the QLQ-CX24 items and no other variables.
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
#' This function returns a total of 9 different scores from the EORTC
#' QLQ-CX24. Scores are calculated according to the official scoring algorithms
#' from the EORTC.
#'
#' In addition to the name of your data frame containing the QLQ-CX24 item
#' responses (\code{df}), you need to tell the function how to find the
#' variables that correspond to the QLQ-CX24 items in \code{df}.  You can do this
#' in 1 of 2 ways:
#'   \enumerate{
#'     \item The first way is to manually provide the item names or locations
#'       using the \code{items} argument.  For example, if your first 10
#'       variables in \code{df} contain demographics, followed by the 24 QLQ-CX24
#'       items \strong{in order} starting with the 11th variable, then you could
#'       use \code{items = 11:34}.
#'     \item The second way only applies if your data frame (\code{df}) contains
#'       \strong{ONLY} the 24 variables corresponding to the 24 QLQ-CX24 items,
#'       in order, with no other non-QLQ-CX24 variables.  In this case, you can
#'       just use the \code{df} argument and omit \code{items}.
#'  }
#'
#'
#'
#' @section How Missing Data is Handled:
#'   The \code{qlq_cx24} function will calculate the scale scores as long as at
#'   least half of the items on the given scale have valid, non-missing item
#'   responses.  Scores calculated in the presence of missing
#'   items are pro-rated so that their theoretical minimum and maximum values
#'   are identical to those from scores calculated from complete data.
#'
#'
#' @return
#' A data frame with all 9 of the QLQ-CX24 scores is returned.  Of the 9
#' scores, 7 are Symptom Scales and 2 are Functional Scales (see below).  Of the
#' 9 scores, only 3 are based on multiple items (the first 3 Symptom Scales),
#' and the other 6 scores are each based on a single item.  All scores are
#' scaled to range from 0-100, even scores based on single items.  Be aware that
#' these single-item scales still have only 4 possible values, even though they
#' are transformed to range from 0-100.  The scale names and numbers of items are
#' listed below.
#'
#' \strong{Symptom Scales (higher is more symptoms, worse functioning)}
#' \itemize{
#'   \item \strong{CX_SE} - Symptom Experience (from 11 items)
#'   \item \strong{CX_BI} - Body Image (from 3 items)
#'   \item \strong{CX_SV} - Sexual/Vaginal Functioning (from 4 items)
#'   \item \strong{CX_LY} - Lymphoedema (from 1 item)
#'   \item \strong{CX_PN} - Peripheral Neuropathy (from 1 item)
#'   \item \strong{CX_MS} - Menopausal Symptoms (from 1 item)
#'   \item \strong{CX_SXW} - Sexual Worry (from 1 item)
#' }
#'
#' \strong{Functional Scales (higher is better functioning)}
#' \itemize{
#'   \item \strong{CX_SXA} - Sexual Activity (from 1 item)
#'   \item \strong{CX_SXE} - Sexual Enjoyment (from 1 item)
#' }
#'
#' Optionally, the data frame can additionally have variables containing the
#' number of valid item responses on each scale for each respondent (if
#' \code{keepNvalid = TRUE}, but this option might be removed in future package
#' updates).
#'
#' @references
#'
#' Greimel E, Kuljanic Vlasic K, Waldenstrom AC et al. on behalf of the EORTC
#' Quality of Life Group. The European Organization for Research and Treatment of
#' Cancer (EORTC) Quality-of-Life questionnaire cervical cancer module - EORTC
#' QLQ-CX24. \emph{Cancer 107} (8): 1812-1822, 2006.
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dat <- PROscorerTools::makeFakeData(n = 10, nitems = 24, prefix = "cx", values = 1:4)
#' qlq_cx24(dat, items = 1:24)
#' }


qlq_cx24 <- function(df, items = NULL, keepNvalid = FALSE) {

  # Check arguments that are unique to qlq_cx24,
  # or that require additional checks not already done by scoreScale().


  #  Get dfItems, a df with only the items -------------------------------------
  dfItems <- PROscorerTools::get_dfItems(df, items )

  # Check that dfItems has 24 items
  if (!PROscorerTools::chk_nitems(dfItems, 24)) {
    stop(paste(strwrap(
      "The QLQ-CX24 has 24 items, but the function found a different number
      of items given the arguments and data you supplied.",
      exdent = 2, width = 65), collapse = "\n"))
  }

  ## Check item ranges:
  ## Not strictly necessary since scoreScale() will check, too.
  ## However, the scoreScale() error msgs would be confusing.
  ## Best to check ranges and give helpful error msgs for specific questionnaires.

  if (!PROscorerTools::chk_values(dfItems = dfItems[1:24],
                                  values = c(1:4, NA))) {
    stop(paste(strwrap(
      "At least one of your items has a value that is not allowed.
      Items should have all integer values between 1 and 4,
      or NA (i.e., missing).",
      exdent = 2, width = 65), collapse = "\n"))
  }

  CX_SE <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = c(1:7, 9, 11:13),
                                        revitems = FALSE,
                                        scalename ="CX_SE" )

  CX_BI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 15:17,
                                        revitems = FALSE,
                                        scalename ="CX_BI" )

  CX_SV <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 20:23,
                                        revitems = FALSE,
                                        scalename ="CX_SV" )

  CX_LY <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 8,
                                        revitems = FALSE,
                                        scalename ="CX_LY" )

  CX_PN <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 10,
                                        revitems = FALSE,
                                        scalename ="CX_PN" )

  CX_MS <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 14,
                                        revitems = FALSE,
                                        scalename ="CX_MS" )

  CX_SXW <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 18,
                                        revitems = FALSE,
                                        scalename ="CX_SXW" )

  CX_SXA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 19,
                                        revitems = FALSE,
                                        scalename ="CX_SXA" )

  CX_SXE <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 24,
                                        revitems = FALSE,
                                        scalename ="CX_SXE" )

  scoreDF <- data.frame(CX_SE,  CX_BI, CX_SV, CX_LY, CX_PN, CX_MS, CX_SXW,
                        CX_SXA, CX_SXE)

  return(scoreDF)
}


