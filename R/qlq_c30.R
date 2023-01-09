#' @title Score the EORTC QLQ-C30 Quality of Life Questionnaire
#'
#' @description Scores the European Organization for Research and Treatment of
#'   Cancer (EORTC) QLQ-C30 Quality of Life Questionnaire (version 3.0).
#'
#' @param df A data frame containing responses to the 30 QLQ-C30 items, and
#'   possibly other variables.
#' @param iprefix Quoted item number prefix.  Quote the letter(s) preceding the
#'   item numbers as they are named in your data frame.  For example, use
#'   \code{iprefix = "q"} if your items are named \code{"q1"}, \code{"q2"}, etc.
#'   Use either this \code{iprefix} argument or the \code{items} argument (but
#'   NOT BOTH) to tell the function which variables in \code{df} are the QLQ-C30
#'   items. See Details for more information.
#' @param items A character vector with the QLQ-C30 item names, or a numeric
#'   vector indicating the column numbers of the QLQ-C30 items in \code{df}.
#'   Use either this \code{items} argument or the \code{iprefix} argument (but
#'   NOT BOTH) to tell the function which variables in \code{df} are the QLQ-C30
#'   items. If \code{items} is omitted, then \code{qlq_c30} will assume that
#'   \code{df} contains \strong{ONLY} the QLQ-C30 items and no other variables.
#'   See Details for more information.
#' @param keepNvalid Logical, whether to return variables containing the
#'   number of valid, non-missing items on each scale for each respondent should
#'   be returned in the data frame with the scale scores.  The default is
#'   \code{FALSE}.  Set to \code{TRUE} to return these variables, which will be
#'   named \code{"scalename_N"} (e.g., \code{QL_N}).  Most users should omit
#'   this argument entirely.  This argument might be removed from future
#'   versions of the package, so please let me know if you think this argument
#'   useful and would rather it remain a part of the function.
#' @param keep_data Logical, whether to keep the original dataframe in the output
#'
#'
#' @details
#' This function returns a total of 16 different scores from the EORTC QLQ-C30
#' (Aaronson et al., 1993), including the new QLQ-C30 Summary Score (Giesinger
#' et al, 2016).  Scores are calculated according to the official scoring
#' algorithms in the EORTC QLQ-C30 Scoring Manual (Fayers et al, 2001).
#'
#' In addition to the name of your data frame containing the QLQ-C30 item
#' responses (\code{df}), you need to tell the function how to find the
#' variables that correspond to the QLQ-C30 items in \code{df}.  You can do this
#' in 1 of 3 different ways:
#'   \enumerate{
#'     \item The easiest way is to use the \code{iprefix} argument.  This
#'       assumes that your items are named using a consistent prefix,
#'       followed by the item number (e.g., 'q1', 'q2', 'q3', etc.).  In this
#'       case, you could use \code{iprefix = 'q'}, and the function will know to
#'       look for items named 'q1' to 'q30' in your data (\code{df}).  Note that
#'       this method will \strong{NOT} work if your items are numbered with
#'       leading zeros for single digit item numbers (e.g., 'q01', 'q02', etc.).
#'     \item The second way is to manually provide the item names or locations
#'       using the \code{items} argument.  For example, if your first 10
#'       variables in \code{df} contain demographics, followed by the 30 QLQ-C30
#'       items \strong{in order} starting with the 11th variable, then you could
#'       use \code{items = 11:40}.
#'     \item The last way only applies if your data frame (\code{df}) contains
#'       \strong{ONLY} the 30 variables corresponding to the 30 QLQ-C30 items,
#'       in order, with no other non-QLQ-C30 variables.  In this case, you can
#'       just use the \code{df} argument and omit \code{iprefix} and
#'       \code{items}.
#'  }
#'
#' You can use EITHER the \code{iprefix} or \code{items} argument, or NEITHER of
#' them (in the case of #3 above).  \strong{But you cannot use both.}
#'
#' @note
#' This function follows the scoring algorithm in the official EORTC QLQ-C30 Scoring
#' Manual (Fayers et al, 2001) exactly, with two exceptions.
#' \itemize{
#'   \item \strong{QLQ-C30 Summary Score} - The QLQ-C30 Summary Score
#'     \code{C30SUMMARY} was developed after the EORTC QLQ-C30 Scoring Manual
#'     was published.  This summary scale was scored according to instructions
#'     on the EORTC website (http://groups.eortc.be/qol/manuals).
#'   \item \strong{Scale Score Names} - The QLQ-C30 Scoring Manual names the
#'     Global Health Status/QoL scale, the Physical Functioning scale, and the
#'     Role Functioning scale 'QL2', 'PF2', and 'RF2', respectively, to indicate
#'     that these are revised versions of these scales.  However, this clashes
#'     with the naming convention that many statisticians use for longitudinal
#'     assessments (e.g., where 'QL2' would be used to indicate the second 'QL'
#'     assessment).  As such, this function drops the '2' suffix from these
#'     scale names.
#' }
#'
#' @section How Missing Data is Handled:
#'   The \code{qlq_c30} function will calculate the scale scores as long as at
#'   least half of the items on the given scale have valid, non-missing item
#'   responses.  The \code{qlq_c30} function will calculate the QLQ-C30 Summary
#'   Score (`C30SUMMARY`) for a respondent only if all 13 scales contributing to
#'   that score are non-missing.  Scores calculated in the presence of missing
#'   items are pro-rated so that their theoretical minimum and maximum values
#'   are identical to those from scores calculated from complete data.
#'
#'
#' @return
#' A data frame with all of the QLQ-C30 scores is returned.  All scores
#'   are scaled to range from 0-100, even scores based on single items.  Be
#'   aware that these single-item scales still have only 4 possible values, even
#'   though they are transformed to range from 0-100.  The scale name and number
#'   of items are listed below.
#'
#' \strong{Global health status/QoL}
#' \itemize{
#'   \item \strong{QL} - Global health status/QoL (revised) (from 2 items)
#' }
#' \strong{Functional Scales (higher is better functioning)}
#' \itemize{
#'   \item \strong{PF} - Physical functioning (from 5 items)
#'   \item \strong{RF} - Role functioning (from 2 items)
#'   \item \strong{EF} - Emotional functioning (from 4 items)
#'   \item \strong{CF} - Cognitive functioning (from 2 items)
#'   \item \strong{SF} - Social functioning (from 2 items)
#' }
#' \strong{Symptom Scales (higher is more symptoms, worse functioning)}
#' \itemize{
#'   \item \strong{FA} - Fatigue (from 3 items)
#'   \item \strong{NV} - Nausea and Vomiting (from 2 items)
#'   \item \strong{PA} - Pain (from 2 items)
#' }
#' \strong{Single-Item Symptom Scores (higher is more symptoms, worse functioning)}
#' \itemize{
#'   \item \strong{DY} - Dyspnoea
#'   \item \strong{SL} - Insomnia
#'   \item \strong{AP} - Appetite Loss
#'   \item \strong{CO} - Constipation
#'   \item \strong{DI} - Diarrhoea
#'   \item \strong{FI} - Financial Difficulties
#' }
#' \strong{QLQ-C30 Summary Score (higher is better functioning, fewer symptoms)}
#' \itemize{
#'   \item \strong{C30SUMMARY} - QLQ-C30 Summary Score, composed by taking mean
#'     of all scores except for QL (Global health status/QoL) and FI (Financial
#'     Difficulties)
#' }
#'
#' Optionally, the data frame can additionally have variables containing the
#' number of valid item responses on each scale for each respondent (if
#' \code{keepNvalid = TRUE}, but this option might be removed in future package
#' updates).
#'
#' @references
#'
#' Aaronson NK, Ahmedzai S, Bergman B, Bullinger M, Cull A, Duez NJ, Filiberti
#' A, Flechtner H, Fleishman SB, Haes JCJM de, Kaasa S, Klee M, Osoba D, Razavi
#' D, Rofe PB, Schraub S, Sneeuw K, Sullivan M, Takeda F (1993). The European
#' Organization for Research and Treatment of Cancer QLQ-C30: A Quality-of-Life
#' Instrument for Use in International Clinical Trials in Oncology. \emph{JNCI J
#' Natl Cancer Inst 85}:365-376.
#'
#' Fayers PM, Aaronson NK, Bjordal K, Groenvold M, Curran D, Bottomley A, on
#' behalf of the EORTC Quality of Life Group. \emph{The EORTC QLQ-C30 Scoring
#' Manual (3rd Edition)}. Published by: European Organisation for Research and
#' Treatment of Cancer, Brussels 2001.
#'
#' Giesinger JM, Kieffer JM, Fayers PM, Groenvold M, Petersen MA, Scott NW,
#' Sprangers MAG, Velikova G, Aaronson NK (2016). Replication and validation of
#' higher order models demonstrated that a summary score for the EORTC QLQ-C30
#' is robust. \emph{Journal of Clinical Epidemiology 69}:79-88.
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dat <- PROscorerTools::makeFakeData(n = 10, nitems = 30, values = 1:4)
#' qlq_c30(dat, 'q')
#' }

qlq_c30 <- function(df, iprefix = NULL, items = NULL, keepNvalid = FALSE, keep_data = FALSE) {

# Check arguments that are unique to qlq_c30,
# or that require additional checks not already done by scoreScale().

# Make sure that 1 or neither of iprefix and items is used.
  if (!is.null(iprefix) & !is.null(items)) {
    stop("Please use either the 'iprefix' or 'items' argument, but not both.")
  }

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
## RAY: Need to write a check for correct number of items when iprefix is used.
##      Do before using get_dfItems().
## Currently:
##      (1) if user has > 30 items with the prefix, function runs normally
##      (2) if user has < 30 items with the prefix, a base R error occurs
## Instead, I want:
##      (1) > 30 items with prefix: warning RE possible data problem
##      (2) < 30 items with prefix: stop with helpful, custom error message
## TODO:
##      After making items (vector of item names), check for matches in names(df)
##        - If not all find a match, stop with a helpful error msg
##      Then make extra item names for nitems+100, check if any names(df) match
##        - If so, give a warning describing the situation
##      OR... Use regex to get all names in df matching iprefix# pattern
##        - Check if length of that vector is <, =, or > nitems.
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##

# If iprefix given, use it to derive items
  if (!is.null(iprefix)) {
    items <- PROscorerTools::makeItemNames(prefix = iprefix, nitems = 30)
  }

#  Get dfItems, a df with only the items -------------------------------------
  dfItems <- PROscorerTools::get_dfItems(df, items )

# Check that dfItems has 30 items
  if (!PROscorerTools::chk_nitems(dfItems, 30)) {
    stop(paste(strwrap(
      "The QLQ-C30 has 30 items, but the function found a different number
      of items given the arguments and data you supplied.",
      exdent = 2, width = 65), collapse = "\n"))
  }
####  (Above seems to work, despite using un-names arguments)
  # if (dim(dfItems)[2] != 30) {
  #   stop(paste(strwrap(
  #     "The QLQ-C30 has 30 items, but the function found a different number
  #     of items given the arguments and data you supplied.",
  #     exdent = 2, width = 65), collapse = "\n"))
  # }


## Check item ranges:
## Not strictly necessary since scoreScale() will check, too.
## However, the scoreScale() error msgs would be confusing.
## Best to check ranges and give helpful error msgs for specific questionnaires.

  if (!PROscorerTools::chk_values(dfItems = dfItems[1:28],
                values = c(1:4, NA))) {
    stop(paste(strwrap(
      "At least one of your items has a value that is not allowed.
      Items 1 to 28 should have all integer values between 1 and 4,
      or NA (i.e., missing).",
      exdent = 2, width = 65), collapse = "\n"))
  }
  if(!PROscorerTools::chk_values(dfItems = dfItems[29:30],
                values = c(1:7, NA))) {
    stop(paste(strwrap(
      "At least one of your items has a value that is not allowed.
      Items 29 and 30 should have all integer values between 1 and 7,
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





  QL <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 7),
                    items = 29:30,
                    revitems = FALSE,
                    scalename ="QL" )

  PF <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 1:5,
                    revitems = TRUE,
                    scalename ="PF" )

  RF <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 6:7,
                    revitems = TRUE,
                    scalename ="RF" )

  EF <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 21:24,
                    revitems = TRUE,
                    scalename ="EF" )

  CF <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = c(20, 25),
                    revitems = TRUE,
                    scalename ="CF" )

  SF <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 26:27,
                    revitems = TRUE,
                    scalename ="SF" )

  FA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = c(10, 12, 18),
                    revitems = FALSE,
                    scalename ="FA" )

  NV <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 14:15,
                    revitems = FALSE,
                    scalename ="NV" )

  PA <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = c(9, 19),
                    revitems = FALSE,
                    scalename ="PA" )

  DY <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 8,
                    revitems = FALSE,
                    scalename ="DY" )

  SL <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 11,
                    revitems = FALSE,
                    scalename ="SL" )

  AP <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 13,
                    revitems = FALSE,
                    scalename ="AP" )

  CO <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 16,
                    revitems = FALSE,
                    scalename ="CO" )

  DI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 17,
                    revitems = FALSE,
                    scalename ="DI" )

  FI <- PROscorerTools::scoreScale( df = dfItems, okmiss = .50,
                    keepNvalid = keepNvalid,
                    minmax = c(1, 4),
                    items = 28,
                    revitems = FALSE,
                    scalename ="FI" )

  scoreDF <- data.frame(QL, PF, RF, EF, CF, SF,
                        FA, NV, PA, DY, SL, AP, CO, DI, FI)

# Calculate QLQ-C30 summary score (QLQTOTAL)
  nms <- c("PF", "RF", "EF", "CF", "SF",
           "FA", "NV", "PA", "DY", "SL", "AP", "CO", "DI")
  QLQTOTAL <- PROscorerTools::scoreScale( df = scoreDF, okmiss = 0,
                  keepNvalid = FALSE,
                  minmax = c(0, 100),
                  type = "mean",
                  items = nms,
                  revitems = c("FA", "NV", "PA", "DY", "SL", "AP", "CO", "DI"),
                  scalename = "QLQTOTAL" )

  scoreDF <- data.frame(scoreDF, QLQTOTAL)
  if(isTRUE(keep_data)){
    scoreDF <- cbind(df, scoreDF)
  }
  return(scoreDF)
}
