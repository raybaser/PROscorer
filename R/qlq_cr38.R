#' @title Score the EORTC QLQ-CR38 Quality of Life Questionnaire
#'
#' @description Scores the European Organization for Research and Treatment of
#'   Cancer (EORTC) QLQ-CR38 Colorectal Cancer Module. (Experimental: This
#'   function was written quickly... please hand score 1 or 2 patients and check
#'   for accuracy)
#'
#' @param df A data frame containing responses to the 38 QLQ-CR38 items, and
#'   possibly other variables.
#' @param items A character vector with the QLQ-CR38 item names, or a numeric
#'   vector indicating the column numbers of the QLQ-CR38 items in \code{df}.
#'   If \code{items} is omitted, then \code{qlq_CR38} will assume that
#'   \code{df} contains \strong{ONLY} the QLQ-CR38 items and no other variables.
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
#' This function returns a total of 12 different scores from the EORTC
#' QLQ-CR38. The scoring instructions for the QLQ-CR38 are no longer available
#' from the EORTC, and the QLQ-CR38 seems to have been superseded by the
#' QLQ-CR29.  This function calculates the QLQ-CR38 scores based on an old SPSS
#' syntax file from the EORTC, and from Sprangers et al. (1999).
#'
#' In addition to the name of your data frame containing the QLQ-CR38 item
#' responses (\code{df}), you need to tell the function how to find the
#' variables that correspond to the QLQ-CR38 items in \code{df}.  You can do this
#' in 1 of 2 ways:
#'   \enumerate{
#'     \item The first way is to manually provide the item names or locations
#'       using the \code{items} argument.  For example, if your first 10
#'       variables in \code{df} contain demographics, followed by the 38 QLQ-CR38
#'       items \strong{in order} starting with the 11th variable, then you could
#'       use \code{items = 11:48}.
#'     \item The second way only applies if your data frame (\code{df}) contains
#'       \strong{ONLY} the 38 variables corresponding to the 38 QLQ-CR38 items,
#'       in order, with no other non-QLQ-CR38 variables.  In this case, you can
#'       just use the \code{df} argument and omit \code{items}.
#'  }
#'
#'
#'
#' @section How Missing Data is Handled:
#'   The \code{qlq_cr38} function will calculate the scale scores as long as at
#'   least half of the items on the given scale have valid, non-missing item
#'   responses.  Scores calculated in the presence of missing
#'   items are pro-rated so that their theoretical minimum and maximum values
#'   are identical to those from scores calculated from complete data.
#'
#'
#' @return
#' A data frame with all 12 of the QLQ-CR38 scores is returned.  Of the 12
#' scores, 4 are Functional Scales and 8 are Symptom Scales (see below).  Of the
#' 8 Symptom Scales, 1 is based on a single item and 7 are multi-item scales.
#' Of the 4 Functional Scales, 2 are multi-item and 2 are based on single items.
#' All scores are scaled to range from 0-100, even scores based on single items.
#' Be aware that these single-item scales still have only 4 possible values,
#' even though they are transformed to range from 0-100.  The scale names and
#' numbers of items are listed below.
#'
#' @note
#' Some of the QLQ-CR38 items/scales are not applicable to all patients.  There
#' are 2 questions for men only, 2 questions for women only, 7 questions only
#' for patients WITHOUT a stoma (colostomy bag), and 7 questions only for
#' patients WITH a stoma.  Patients will have
#'
#' \strong{Functional Scales (higher is better functioning)}
#' \itemize{
#'   \item \strong{CR_BI} - Body Image (3 items)
#'   \item \strong{CR_SX} - Sexual Functioning (2 items)
#'   \item \strong{CR_SE} - Sexual Enjoyment (1 item)
#'   \item \strong{CR_FU} - Future Perspective (1 item)
#' }
#'
#' \strong{Symptom Scales (higher is more symptoms, worse functioning)}
#' \itemize{
#'   \item \strong{CR_MI}  - Micturition Problems (3 items)
#'   \item \strong{CR_GI}  - Gastrointestinal Tract Symptoms (5 items)
#'   \item \strong{CR_CT}  - Chemotherapy Side-Effects (3 items)
#'   \item \strong{CR_DF}  - Problems with Defacation (Only for Pts WITHOUT a stoma) (7 items)
#'   \item \strong{CR_STO} - Stoma-Related Problems (Only for Pts w/stoma) (7 items)
#'   \item \strong{CR_MSX} - Male Sexual Problems (2 items)
#'   \item \strong{CR_FSX} - Female Sexual Problems (2 items)
#'   \item \strong{CR_WL}  - Weigt Loss (1 item)
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
#' Sprangers, M. A. G., te Velde, A., & Aaronson, N. K. (1999). The construction
#' and testing of the EORTC colorectal cancer-specific quality of life
#' questionnaire module (QLQ-CR38). \emph{European Journal of Cancer, 35}(2),
#' 238–247. https://doi.org/10.1016/S0959-8049(98)00357-8

#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dat <- PROscorerTools::makeFakeData(n = 10, nitems = 38, prefix = "cr", values = 1:4)
#' qlq_cr38(dat, items = 1:38)
#' }


qlq_cr38 <- function(df, items = NULL, keepNvalid = FALSE) {

  # Check arguments that are unique to qlq_cr38,
  # or that require additional checks not already done by scoreScale().


  #  Get dfItems, a df with only the items -------------------------------------
  dfItems <- PROscorerTools::get_dfItems(df, items )

  # Check that dfItems has 38 items
  if (!PROscorerTools::chk_nitems(dfItems, 38)) {
    stop(paste(strwrap(
      "The QLQ-CR38 has 38 items, but the function found a different number
      of items given the arguments and data you supplied.",
      exdent = 2, width = 65), collapse = "\n"))
  }

  ## Check item ranges:
  ## Not strictly necessary since scoreScale() will check, too.
  ## However, the scoreScale() error msgs would be confusing.
  ## Best to check ranges and give helpful error msgs for specific questionnaires.

  if (!PROscorerTools::chk_values(dfItems = dfItems[1:38],
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

  # Functional scales
  CR_BI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 13:15,
                                       revitems = TRUE,
                                       scalename ="CR_BI" )

  CR_SX <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                        keepNvalid = keepNvalid,
                                        minmax = c(1, 4),
                                        items = 17:18,
                                        revitems = FALSE,
                                        scalename ="CR_SX" )

  CR_SE <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 19,
                                       revitems = FALSE,
                                       scalename ="CR_SE" )

  CR_FU <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 16,
                                       revitems = TRUE,
                                       scalename ="CR_FU" )

  # Symptom scales
  CR_MI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 1:3,
                                       revitems = FALSE,
                                       scalename ="CR_MI" )

  CR_GI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 4:8,
                                       revitems = FALSE,
                                       scalename ="CR_GI" )


  CR_CT <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 10:12,
                                       revitems = FALSE,
                                       scalename ="CR_CT" )


  CR_DF <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 25:31,
                                       revitems = FALSE,
                                       scalename ="CR_DF" )


  CR_STO <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 32:38,
                                       revitems = FALSE,
                                       scalename ="CR_STO" )


  CR_MSX <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 20:21,
                                       revitems = FALSE,
                                       scalename ="CR_MSX" )


  CR_FSX <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 22:23,
                                       revitems = FALSE,
                                       scalename ="CR_FSX" )


  CR_WL <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                                       keepNvalid = keepNvalid,
                                       minmax = c(1, 4),
                                       items = 9,
                                       revitems = FALSE,
                                       scalename ="CR_WL" )

  scoreDF <- data.frame(CR_BI, CR_SX, CR_SE, CR_FU,
                        CR_MI, CR_GI, CR_CT, CR_DF, CR_STO, CR_MSX, CR_FSX, CR_WL)

  return(scoreDF)
}
