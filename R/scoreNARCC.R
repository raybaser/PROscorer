#' @title Score the Cognitive Causation (CC) and Negative Affect in Risk (NAR)
#'   scales
#'
#' @description Scores the Cognitive Causation (CC) and Negative Affect in Risk
#'   (NAR) scales, two scales measuring intuitive elements of cancer risk
#'   perception.
#'
#' @param df A data frame containing responses to the CC and/or NAR items, and
#'   possibly other variables.
#' @param items (optional) A character vector with the CC or NAR item names, or
#'   a numeric vector indicating the column numbers of the CC or NAR items in
#'   \code{df}.  If \code{items} is omitted, then \code{scoreNARCC} will assume
#'   that \code{df} contains only the items to be scored (either CC or NAR
#'   items) and no non-scored variables.
#' @param whichScale Either \code{"CC"} or \code{"NAR"}, the scale you wish to
#'   score.
#' @param minmax A vector of 2 integers with format \code{c(itemMin, itemMax)},
#'   indicating the minimum and maximum possible item responses.  The default
#'   value is \code{c(0, 3)}, and assumes that the item responses are coded from
#'   0 to 3.  If, instead, your item responses are coded from 1 to 4, then enter
#'   \code{c(1, 4)} for this argument.
#' @param okmiss The maximum proportion of items on \code{whichScale} that a
#'   respondent is allowed to have missing and still have their non-missing
#'   items scored (and prorated). If the proportion of missing items for a
#'   respondent is greater than \code{okmiss}, then the respondent will be
#'   assigned a value of \code{NA} for their scale score.  The default value is
#'   \code{0.50}, and this generally should not be changed.
#' @param keepNvalid Logical value indicating whether a variable containing the
#'   number of valid, non-missing items for each respondent should be returned
#'   in a data frame with the scale score.  The default is \code{FALSE}.  Set to
#'   \code{TRUE} to return this variable, which will be named
#'   \code{"whichScale_N"} (with whatever name you gave to the \code{whichScale}
#'   argument).
#'
#' @details
#'
#' @references
#' Cohen, P, Cohen, J, Aiken, LS, & West, SG. (1999). The problem of units and
#' the circumstance for POMP. \emph{Multivariate Behavioral Research}, 34(3),
#' 315–346.
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
#' scoreNARCC(nardat, whichScale = "NAR")
#'
#' # The ccdat data frame contains an "ID" variable, so need to use "items" arg
#' names(ccdat)
#'
#' # The "items" argument can be either the numeric vector indexing
#' #     the location of the items in df,
#' #     or a character vector of the item names
#' scoreNARCC(ccdat, items = 2:8, whichScale = "CC")
#'
#' cc_names <- c("cc1", "cc2", "cc3", "cc4", "cc5", "cc6", "cc7")
#' scoreNARCC(ccdat, items = cc_names, whichScale = "CC")
scoreNARCC <- function(df,
                        items = NULL,
                        whichScale,
                        minmax = c(0, 3),
                        okmiss = 0.50,
                        keepNvalid = FALSE
                        ) {

  # Check arguments that are unique to scoreNARCC,
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
      stop(msgWrap(
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
      stop(msgWrap(
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

