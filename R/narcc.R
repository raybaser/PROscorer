#' @title Score the Cognitive Causation (CC) and Negative Affect in Risk (NAR)
#'   scales
#'
#' @description Scores the Cognitive Causation (CC) and Negative Affect in Risk
#'   (NAR) scales, two scales measuring intuitive elements of cancer risk
#'   perception (see references).
#'
#' @param df A data frame containing responses to the CC and/or NAR items, and
#'   possibly other variables.
#' @param items (optional) A character vector with the CC or NAR item names, or
#'   a numeric vector indicating the column numbers of the CC or NAR items in
#'   \code{df}.  If \code{items} is omitted, then \code{narcc} will assume
#'   that \code{df} contains \strong{ONLY} the items to be scored (either CC or
#'   NAR items) and no non-scored variables.
#' @param whichScale (required) Either \code{"CC"} or \code{"NAR"}, the scale
#'   you wish to score.
#' @param minmax A vector of 2 integers with format \code{c(itemMin, itemMax)},
#'   indicating the minimum and maximum possible item responses.  The default
#'   value is \code{c(0, 3)}, and assumes that the item responses are coded from
#'   0 to 3.  If, instead, your item responses are coded from 1 to 4, then enter
#'   \code{c(1, 4)} for this argument.
#' @param okmiss (optional) The maximum proportion of items on \code{whichScale}
#'   that a respondent is allowed to have missing and still have their
#'   non-missing items scored (and prorated). If the proportion of missing items
#'   for a respondent is greater than \code{okmiss}, then the respondent will be
#'   assigned a value of \code{NA} for their scale score.  The default value is
#'   \code{0.50}, and this generally should not be changed.
#' @param keepNvalid (optional) Logical value indicating whether a variable
#'   containing the number of valid, non-missing items for each respondent
#'   should be returned in a data frame with the scale score.  The default is
#'   \code{FALSE}.  Set to \code{TRUE} to return this variable, which will be
#'   named \code{"whichScale_N"} (with whatever name you gave to the
#'   \code{whichScale} argument).  Most users should omit this argument
#'   entirely.  This argument might be removed from future versions of the
#'   package, so please let me know if you think this argument useful and would
#'   rather it remain a part of the function.
#'
#' @details
#' The CC scale originally contained 10 items (Hay et al., 2014).  Later,
#' evidence that 3 of the items might be measurement non-invariant across
#' important subgroups led to the recommendation to omit these 3 items and score
#' a 7-item version of the CC scale (Baser et al., 2016).  When \code{whichScale
#' = "CC"} the \code{narcc} function will accept and score either 7 or 10
#' CC items, although the 7-item version is recommended.  The NAR scale has 6
#' items, and the \code{narcc} function will accept only 6 NAR items when
#' \code{whichScale = "NAR"}.
#'
#' If you want to score both the CC and NAR scales, then you need to run the
#' \code{narcc} function twice, once for CC and again for NAR.
#'
#' @note
#' The \code{narcc} function assumes that your item data are numerically
#' coded from 0 to 3 (i.e., with 0 = "Strongly Disagree" and 3 = "Strongly
#' Agree").  However, your item data might instead be coded from 1 to 4.  If
#' this is the case, you MUST let the \code{narcc} function know this by
#' using the \code{minmax} argument, specifically, \code{minmax = c(1, 4)}.
#'
#' @return
#' A data frame containing a variable containing the scored scale, named either
#' \code{"CC"} or \code{"NAR"}.  Scores are scales to have range 0 to 100.
#'
#' Optionally, the data frame can additionally have a variable containing the
#' number of valid item responses on the scale for each respondent (if
#' \code{keepNvalid = TRUE}, but this option might be removed in future package
#' updates).
#'
#' @references
#' Hay, JL, Baser, R, Weinstein, ND, Li, Y, Primavera, L, & Kemeny, MM. (2014).
#' Examining intuitive risk perceptions for cancer in diverse populations.
#' \emph{Health, Risk & Society, 16}(3), 227-242.
#'
#' Baser, RE, Li, Y, Brennessel, D, Kemeny, MM, & Hay, JL. (2016). Measurement
#' Invariance of Intuitive Cancer Risk Perceptions Across Diverse Populations:
#' The Cognitive Causation and Negative Affect in Risk Scales. \emph{Journal of
#' Health Psychology}; In Submission.
#'
#' @export
#'
#' @examples
#' # Make fake data for the example
#' nardat <- PROscorerTools::makeFakeData(nitems = 6, values = 0:3,
#'                                        propmiss = 0.40, prefix = "nar")
#' ccdat <- PROscorerTools::makeFakeData(nitems = 7, values = 0:3,
#'                                       propmiss = 0.40, prefix = "cc",
#'                                       id = TRUE)
#'
#' # The nardat data frame contains ONLY NAR items, so can omit "items" argument
#' narcc(nardat, whichScale = "NAR")
#'
#' # The ccdat data frame contains an "ID" variable, so need to use "items" arg
#' names(ccdat)
#'
#' # The "items" argument can be either:
#' #     (1) the numeric vector indexing the location of the items in df, or
#' #     (2) a character vector of the item names
#' narcc(ccdat, items = 2:8, whichScale = "CC")
#'
#' cc_names <- c("cc1", "cc2", "cc3", "cc4", "cc5", "cc6", "cc7")
#' narcc(ccdat, items = cc_names, whichScale = "CC")
narcc <- function(df,
                        items = NULL,
                        whichScale,
                        minmax = c(0, 3),
                        okmiss = 0.50,
                        keepNvalid = FALSE
                        ) {

  # Check arguments that are unique to narcc,
  # or that require additional checks not already done by scoreScale().

  #  Get dfItems, a df with only the items -------------------------------------
  dfItems <- PROscorerTools::get_dfItems(df, items )

  #  Check that items all within minmax range ----------------------------------
  #  [NOT NECESSARY.  Should already be handled by scoreScale().]
  # if (!PROscorerTools::chk_values(dfItems = dfItems,
  #                                 values = minmax[1]:minmax[2])) {
  #   stop(msgWrap(
  #     msg = "At least one of your items has a value outside the range you
  #           gave to 'minmax' (or outside of c(0, 3), if you omitted the
  #           'minmax' argument)."))
  # }

  #  Check that whichScale is not missing and has valid value ------------------
  if (missing(whichScale) || !(whichScale %in% c("CC", "cc", "NAR", "nar"))) {
    stop("Please use the 'whichScale' argument to indicate whether
        you want to score the 'CC' or 'NAR' scale.")
  }

  if (whichScale %in% c("CC", "cc")) {
    # - Is nitems = 10 or 7 for CC?
    if (!PROscorerTools::chk_nitems(dfItems, nitems = 10) &&
        !PROscorerTools::chk_nitems(dfItems, nitems = 7)) {
      stop(PROscorerTools::msgWrap(
        msg = "Your number of items is not equal to 7 or 10.
        Cognitive Causation (CC) has either 7 or 10 items,
        depending on the version you are using.
        If this is not the case, please contact Ray Baser with your details."))
    }

    out <- PROscorerTools::scoreScale(df = df,
                                      items = items,
                                      minmax = minmax,
                                      okmiss = okmiss,
                                      type = "pomp",
                                      scalename ="CC",
                                      keepNvalid = keepNvalid)

  } else if (whichScale %in% c("NAR", "nar")) {

    # - Is nitems = 6 for NAR?
    if (!PROscorerTools::chk_nitems(dfItems, nitems = 6)) {
      stop(PROscorerTools::msgWrap(
        msg = "Your number of items is not equal to 6.
        Negative Affect in Risk (NAR) has 6 items.
        If this is not the case, please contact Ray Baser with your details."))
    }

    out <- PROscorerTools::scoreScale(df = df,
                                      items = items,
                                      minmax = minmax,
                                      okmiss = okmiss,
                                      type = "pomp",
                                      scalename ="NAR",
                                      keepNvalid = keepNvalid)

  }

  return(out)
}

