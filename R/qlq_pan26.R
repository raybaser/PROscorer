#' @title Score the EORTC QLQ-PAN26 Quality of Life Questionnaire
#'
#' @description Scores the European Organization for Research and Treatment of
#'   Cancer (EORTC) QLQ-PAN26 Pancreatic Cancer Module. (Experimental: This
#'   function was written quickly... please hand score 1 or 2 patients and check
#'   for accuracy)
#'
#' @param df A data frame containing responses to the 26 QLQ-PAN26 items, and
#'   possibly other variables.
#' @param items A character vector with the QLQ-PAN26 item names, or a numeric
#'   vector indicating the column numbers of the QLQ-PAN26 items in \code{df}.
#'   If \code{items} is omitted, then \code{qlq_PAN26} will assume that
#'   \code{df} contains \strong{ONLY} the QLQ-PAN26 items and no other variables.
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
#' This function returns a total of 17 different scores from the EORTC
#' QLQ-PAN26. Scores are calculated according to the official scoring algorithms
#' from the EORTC.  At the time this scoring function was written (April 2022),
#' the QLQ-PAN26 had completed Phase 3 testing; however, the official scoring
#' instructions from the EORTC warned that this scaling structure is still
#' preliminary and may change in the future.
#'
#' In addition to the name of your data frame containing the QLQ-PAN26 item
#' responses (\code{df}), you need to tell the function how to find the
#' variables that correspond to the QLQ-PAN26 items in \code{df}.  You can do this
#' in 1 of 2 ways:
#'   \enumerate{
#'     \item The first way is to manually provide the item names or locations
#'       using the \code{items} argument.  For example, if your first 10
#'       variables in \code{df} contain demographics, followed by the 26 QLQ-PAN26
#'       items \strong{in order} starting with the 11th variable, then you could
#'       use \code{items = 11:36}.
#'     \item The second way only applies if your data frame (\code{df}) contains
#'       \strong{ONLY} the 26 variables corresponding to the 26 QLQ-PAN26 items,
#'       in order, with no other non-QLQ-PAN26 variables.  In this case, you can
#'       just use the \code{df} argument and omit \code{items}.
#'  }
#'
#'

#' @note
#' \strong{As of April 15, 2022, there is an error in the official PAN26 scoring
#' instructions from the EORTC.}
#' The first page of the official PAN26 scoring instructions from the EORTC
#' indicates that the two items comprising the SA (Satisfaction with health
#' care) functional scale should be reverse scored.  In the instructions, these
#' two items are numbered as 53 and 54.  However, the second page of the
#' instructions indicates items 55 and 56 should be reversed (see bullet, "(1)
#' Raw score" in the "Principle for scoring" subsection).  Items 55 and 56
#' comprise the SX (Sexuality) functional scale.
#' \strong{The items that need to be reversed are the SX items (55 and 56), NOT
#' the SA items (not 53 and 54).}  This function correctly reverses the SX items
#' instead of the SA items.
#'
#' @section How Missing Data is Handled:
#'   The \code{qlq_PAN26} function will calculate the scale scores as long as at
#'   least half of the items on the given scale have valid, non-missing item
#'   responses.  Scores calculated in the presence of missing
#'   items are pro-rated so that their theoretical minimum and maximum values
#'   are identical to those from scores calculated from complete data.
#'
#'
#' @return
#' A data frame with all 17 of the QLQ-PAN26 scores is returned.  Of the 17
#' scores, 15 are Symptom Scales and 2 are Functional Scales (see below).  Of
#' the 15 Symptom Scales, 10 are based on a single item each.  All scores are
#' scaled to range from 0-100, even scores based on single items.  Be aware that
#' these single-item scales still have only 4 possible values, even though they
#' are transformed to range from 0-100.  The scale names and numbers of items are
#' listed below.
#'
#' \strong{Symptom Scales (higher is more symptoms, worse functioning)}
#' \itemize{
#'   \item \strong{PAN_PP} - Pancreatic pain (from 4 items)
#'   \item \strong{PAN_BF} - Bloating (from 1 item)
#'   \item \strong{PAN_DS} - Digestive symptoms (from 2 items)
#'   \item \strong{PAN_TA} - Taste (from 1 item)
#'   \item \strong{PAN_ID} - Indigestion (from 1 item)
#'   \item \strong{PAN_FL} - Flatulence (from 1 item)
#'   \item \strong{PAN_WL} - Weight loss (from 1 item)
#'   \item \strong{PAN_WE} - Weakness arms and legs (from 1 item)
#'   \item \strong{PAN_DM} - Dry mouth (from 1 item)
#'   \item \strong{PAN_LI} - Hepatic symptoms (from 2 items)
#'   \item \strong{PAN_BO} - Altered bowel habit (from 2 items)
#'   \item \strong{PAN_BI} - Body image (from 2 item)
#'   \item \strong{PAN_SE} - Troubled with side-effects(from 1 item)
#'   \item \strong{PAN_FU} - Future Worries (from 1 item)
#'   \item \strong{PAN_PL} - Planning of activities (from 1 item)
#' }
#'
#' \strong{Functional Scales (higher is better functioning)}
#' \itemize{
#'   \item \strong{PAN_SA} - Satisfaction with health care (from 2 items)
#'   \item \strong{PAN_SX} - Sexuality (from 2 items)
#' }
#'
#' Optionally, the data frame can additionally have variables containing the
#' number of valid item responses on each scale for each respondent (if
#' \code{keepNvalid = TRUE}, but this option might be removed in future package
#' updates).
#'
#' @references
#'
#' Fitzsimmons D, Johnson CD, George S, et al. Development of a disease specific
#' quality of life (QoL) questionnaire module to supplement the EORTC core
#' cancer QoL questionnaire, the QLQ-PAN26 in patients with pancreatic cancer.
#' \emph{Eur. J. Cancer 35}: 939-941, 1999.
#'
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dat <- PROscorerTools::makeFakeData(n = 10, nitems = 26, prefix = "pan", values = 1:4)
#' qlq_pan26(dat, items = 1:26)
#' }


qlq_pan26 <- function(df, items = NULL, keepNvalid = FALSE) {

  # Check arguments that are unique to qlq_pan26,
  # or that require additional checks not already done by scoreScale().


  #  Get dfItems, a df with only the items -------------------------------------
  dfItems <- PROscorerTools::get_dfItems(df, items )

  # Check that dfItems has 26 items
  if (!PROscorerTools::chk_nitems(dfItems, 26)) {
    stop(paste(strwrap(
      "The QLQ-PAN26 has 26 items, but the function found a different number
      of items given the arguments and data you supplied.",
      exdent = 2, width = 65), collapse = "\n"))
  }

  ## Check item ranges:
  ## Not strictly necessary since scoreScale() will check, too.
  ## However, the scoreScale() error msgs would be confusing.
  ## Best to check ranges and give helpful error msgs for specific questionnaires.

  if (!PROscorerTools::chk_values(dfItems = dfItems[1:26],
                                  values = c(1:4, NA))) {
    stop(paste(strwrap(
      "At least one of your items has a value that is not allowed.
      Items should have all integer values between 1 and 4,
      or NA (i.e., missing).",
      exdent = 2, width = 65), collapse = "\n"))
  }

  ##----------------------------------------------------------------------------##
  ##----------------------------------------------------------------------------##
  ## Warn if any items do not contain the full range of possible values
  ## Below was in PROscorerTools::chkstop_minmax(), but doesn't make sense when
  ##   triggered inside a PROscorer function.  Write a custom check for
  ##   qlq_c30 for now, and later write helper fn to do this more seamlessly.
  ##----------------------------------------------------------------------------##
  ##----------------------------------------------------------------------------##
  #  if (imin < min(dfItems, na.rm = TRUE) ) {
  #    warning(paste(strwrap(sprintf(
  #      "The lower bound you gave to 'minmax', %s, is smaller than the
  #      minimum item response observed in the data you provided to 'df'.
  #      Please double-check that you gave the correct lower bound to 'minmax'
  #      (it should be the value of the lowest possible item response),
  #      and that the item responses are coded correctly in your data.
  #      If both are correct, you can ignore this warning.", imin),
  #      exdent = 2),
  #      collapse = "\n"))
  #  }
  #  if (imax > max(dfItems, na.rm = TRUE) ) {
  #    warning(paste(strwrap(sprintf(
  #      "The upper bound you gave to 'minmax', %s, is larger than the
  #      largest item response observed in the data you provided to 'df'.
  #      Please double-check that you gave the correct upper bound to 'minmax'
  #      (it should be the value of the highest possible item response),
  #      and that the item responses are coded correctly in your data.
  #      If both are correct, you can ignore this warning.", imax),
  #      exdent = 2),
  #      collapse = "\n"))
  #  }
  ##----------------------------------------------------------------------------##
  ##----------------------------------------------------------------------------##




  PAN_PP <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = c(1, 3:5),
                                    revitems = FALSE,
                                    scalename ="PAN_PP" )

  PAN_BF <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 2,
                                    revitems = FALSE,
                                    scalename ="PAN_BF" )

  PAN_DS <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 6:7,
                                    revitems = FALSE,
                                    scalename ="PAN_DS" )

  PAN_TA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 8,
                                    revitems = FALSE,
                                    scalename ="PAN_TA" )

  PAN_ID <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 9,
                                    revitems = FALSE,
                                    scalename ="PAN_ID" )

  PAN_FL <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 10,
                                    revitems = FALSE,
                                    scalename ="PAN_FL" )

  PAN_WL <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 11,
                                    revitems = FALSE,
                                    scalename ="PAN_WL" )

  PAN_WE <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 12,
                                    revitems = FALSE,
                                    scalename ="PAN_WE" )

  PAN_DM <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 13,
                                    revitems = FALSE,
                                    scalename ="PAN_DM" )

  PAN_LI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 14:15,
                                    revitems = FALSE,
                                    scalename ="PAN_LI" )

  PAN_BO <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 16:17,
                                    revitems = FALSE,
                                    scalename ="PAN_BO" )

  PAN_BI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 18:19,
                                    revitems = FALSE,
                                    scalename ="PAN_BI" )

  PAN_SE <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 20,
                                    revitems = FALSE,
                                    scalename ="PAN_SE" )

  PAN_FU <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 21,
                                    revitems = FALSE,
                                    scalename ="PAN_FU" )

  PAN_PL <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                    keepNvalid = keepNvalid,
                                    minmax = c(1, 4),
                                    items = 22,
                                    revitems = FALSE,
                                    scalename ="PAN_PL" )


  # Functional scales
  PAN_SA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 23:24,
                                        revitems = FALSE,
                                        scalename ="PAN_SA" )

  PAN_SX <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 25:26,
                                        revitems = TRUE,
                                        scalename ="PAN_SX" )

  scoreDF <- data.frame(PAN_PP, PAN_BF, PAN_DS, PAN_TA, PAN_ID, PAN_FL, PAN_WL,
                        PAN_WE, PAN_DM, PAN_LI, PAN_BO, PAN_BI, PAN_SE, PAN_FU,
                        PAN_PL, PAN_SA, PAN_SX)

  return(scoreDF)
}
