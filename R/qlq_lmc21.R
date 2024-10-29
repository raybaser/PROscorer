#' @title Score the EORTC QLQ-LMC21 Quality of Life Questionnaire
#'
#' @description Scores the European Organization for Research and Treatment of
#'   Cancer (EORTC) QLQ-LMC21 Colorectal Liver Cancer Module.
#'
#' @param df A data frame containing responses to the 21 QLQ-LMC21 items, and
#'   possibly other variables.
#' @param items A character vector with the QLQ-LMC21 item names, or a numeric
#'   vector indicating the column numbers of the QLQ-LMC21 items in \code{df}.
#'   If \code{items} is omitted, then \code{qlq_lmc21} will assume that
#'   \code{df} contains \strong{ONLY} the QLQ-LMC21 items and no other variables.
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
#' This function returns a total of 13 different scores from the EORTC
#' QLQ-LMC21. Scores are calculated according to the official scoring algorithms
#' from the EORTC.
#'
#' In addition to the name of your data frame containing the QLQ-LMC21 item
#' responses (\code{df}), you need to tell the function how to find the
#' variables that correspond to the QLQ-LMC21 items in \code{df}.  You can do this
#' in 1 of 2 ways:
#'   \enumerate{
#'     \item The first way is to manually provide the item names or locations
#'       using the \code{items} argument.  For example, if your first 10
#'       variables in \code{df} contain demographics, followed by the 21 QLQ-LMC21
#'       items \strong{in order} starting with the 11th variable, then you could
#'       use \code{items = 11:36}.
#'     \item The second way only applies if your data frame (\code{df}) contains
#'       \strong{ONLY} the 21 variables corresponding to the 21 QLQ-LMC21 items,
#'       in order, with no other non-QLQ-LMC21 variables.  In this case, you can
#'       just use the \code{df} argument and omit \code{items}.
#'  }
#'
#'
#'
#' @section How Missing Data is Handled:
#'   The \code{qlq_lmc21} function will calculate the scale scores as long as at
#'   least half of the items on the given scale have valid, non-missing item
#'   responses.  Scores calculated in the presence of missing
#'   items are pro-rated so that their theoretical minimum and maximum values
#'   are identical to those from scores calculated from complete data.
#'
#'
#' @return
#' A data frame with all 13 of the QLQ-LMC21 scores is returned.  Of the 13
#' scores, all 13 are Symptom Scales (see below).  Of
#' the 13 Symptom Scales, 9 are based on a single item each.  All scores are
#' scaled to range from 0-100, even scores based on single items.  Be aware that
#' these single-item scales still have only 4 possible values, even though they
#' are transformed to range from 0-100.  The scale names and numbers of items are
#' listed below.
#'
#' \strong{Symptom Scales (higher is more symptoms, worse functioning)}
#' \itemize{
#'   \item \strong{LMC_NP} - Nutritional problems (from 2 items)
#'   \item \strong{LMC_FA} - Fatigue (from 3 items)
#'   \item \strong{LMC_PA} - Pain (from 3 items)
#'   \item \strong{LMC_EP} - Emotional problems (from 4 items)
#'   \item \strong{LMC_WL} - Weight loss (from 1 item)
#'   \item \strong{LMC_TA} - Taste (from 1 item)
#'   \item \strong{LMC_DM} - Dry mouth (from 1 item)
#'   \item \strong{LMC_SM} - Sore mouth/tongue (from 1 item)
#'   \item \strong{LMC_PN} - Peripheral neuropathy (from 1 item)
#'   \item \strong{LMC_JA} - Jaundice (from 1 item)
#'   \item \strong{LMC_FR} - Contact with friends (from 1 item)
#'   \item \strong{LMC_FE} - Talking about feelings (from 1 item)
#'   \item \strong{LMC_SX} - Sex life (from 1 item)
#' }
#'
#'
#' Optionally, the data frame can additionally have variables containing the
#' number of valid item responses on each scale for each respondent (if
#' \code{keepNvalid = TRUE}, but this option might be removed in future package
#' updates).
#'
#' @references
#'
#' Blazeby JM, Fayers P, Conroy T, et al. Validation of the EORTC QLQ-LCM21
#' Questionnaire for Assessment of Patient-Reported Outcomes During Treatment of
#' Colorectal Liver Metastases. \emph{Br J Surg 96}:291-298, 2009.
#'
#' Kavadas V, Blazeby JM, et al.  Development of an EORTC disease-specific
#' quality of life questionnaire for use in patients with liver metastases from
#' colorectal cancer. \emph{Eur J Cancer}. 2003 Jun;39(9):1259-63.
#'
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dat <- PROscorerTools::makeFakeData(n = 10, nitems = 21, prefix = "lmc", values = 1:4)
#' qlq_lmc21(dat, items = 1:21)
#' }


qlq_lmc21 <- function(df, items = NULL, keepNvalid = FALSE) {

  # Check arguments that are unique to qlq_lmc21,
  # or that require additional checks not already done by scoreScale().


  #  Get dfItems, a df with only the items -------------------------------------
  dfItems <- PROscorerTools::get_dfItems(df, items )

  # Check that dfItems has 21 items
  if (!PROscorerTools::chk_nitems(dfItems, 21)) {
    stop(paste(strwrap(
      "The QLQ-LMC21 has 21 items, but the function found a different number
      of items given the arguments and data you supplied.",
      exdent = 2, width = 65), collapse = "\n"))
  }

  ## Check item ranges:
  ## Not strictly necessary since scoreScale() will check, too.
  ## However, the scoreScale() error msgs would be confusing.
  ## Best to check ranges and give helpful error msgs for specific questionnaires.

  if (!PROscorerTools::chk_values(dfItems = dfItems[1:21],
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







  LMC_NP <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = c(1, 2),
                                        revitems = FALSE,
                                        scalename ="LMC_NP" )

  LMC_FA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = c(7, 13, 14),
                                        revitems = FALSE,
                                        scalename ="LMC_FA" )

  LMC_PA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = c(9, 10, 12),
                                        revitems = FALSE,
                                        scalename ="LMC_PA" )

  LMC_EP <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = c(17, 18, 19, 20),
                                        revitems = FALSE,
                                        scalename ="LMC_EP" )



  LMC_WL <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 3,
                                        revitems = FALSE,
                                        scalename ="LMC_WL" )

  LMC_TA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 4,
                                        revitems = FALSE,
                                        scalename ="LMC_TA" )

  LMC_DM <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 5,
                                        revitems = FALSE,
                                        scalename ="LMC_DM" )

  LMC_SM <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 6,
                                        revitems = FALSE,
                                        scalename ="LMC_SM" )

  LMC_PN <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 8,
                                        revitems = FALSE,
                                        scalename ="LMC_PN" )

  LMC_JA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 11,
                                        revitems = FALSE,
                                        scalename ="LMC_JA" )

  LMC_FR <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 15,
                                        revitems = FALSE,
                                        scalename ="LMC_FR" )

  LMC_FE <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 16,
                                        revitems = FALSE,
                                        scalename ="LMC_FE" )


  LMC_SX <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 21,
                                        revitems = FALSE,
                                        scalename ="LMC_SX" )

  scoreDF <- data.frame(LMC_NP, LMC_FA, LMC_PA, LMC_EP, LMC_WL, LMC_TA, LMC_DM,
                        LMC_SM, LMC_PN, LMC_JA, LMC_FR, LMC_FE, LMC_SX)

  return(scoreDF)
}
