#' @title Score the Female Sexual Function Index (FSFI)
#'
#' @description Scores the Female Sexual Function Index (FSFI)
#'
#' @param df A data frame containing responses to the 19 FSFI items, and
#'   possibly other variables.
#' @param iprefix Item number prefix.  Quote the letter(s) preceding the FSFI
#'   item numbers as they are named in your data frame.  If this argument is
#'   omitted, the function will assume that your items are named \code{"fsfi1"},
#'   \code{"fsfi2"}, etc.
#' @param keepNvalid Logical, whether to return variables containing the
#'   number of valid, non-missing items on each scale for each respondent should
#'   be returned in the data frame with the scale scores.  The default is
#'   \code{FALSE}.  Set to \code{TRUE} to return these variables, which will be
#'   named \code{"scalename_N"} (e.g., \code{fsfi_pain_N}).  Most users should
#'   omit this argument entirely.  This argument might be removed from future
#'   versions of the package, so please let me know if you think this argument
#'   useful and would rather it remain a part of the function.
#'
#'
#' @details
#' This function returns the 6 subscale scores and the FSFI Total score (Rosen
#' et al., 2000), as well as an indicator variable flagging respondents with FSFI
#' Total scores suggestive of clinically significant levels of sexual
#' dysfunction (i.e., \code{fsfi_tot <= 26.55}; Wiegel et al., 2005).
#'
#' The FSFI is intended to measure the sexual function of recently sexually
#' active women (Rosen et al., 2000), and strong evidence suggests it may not be
#' a valid measure of sexual function in women with little or no recent sexual
#' activity (e.g., see Baser et al., 2012).
#'
#' As such the \code{fsfi} function also returns two variables
#' (\code{fsfi_nzero15} and \code{fsfi_sexactive01}) that can be used to
#' evaluate whether respondents have been sufficiently sexually active for the
#' FSFI to be a valid assessment of their sexual function.  These variables are
#' based on the fact that 15 of the 19 FSFI items have a response option of "no
#' sexual activity" or "did not attempt intercourse", which corresponds to an
#' item score of \code{0}.  Specifically, the \code{fsfi_nzero15} variable
#' contains the number of items with responses of \code{0} or \code{NA} (out of
#' those 15 items that have a response option indicating "no sexual activity").
#' Missing responses (i.e., \code{NA}) are included in this count because
#' respondents with no relevant sexual activity often skip these items.  The
#' \code{fsfi_sexactive01} variable is a rough indicator that a respondent was
#' sufficiently sexually active for the FSFI to be a valid assessment of their
#' sexual function.  It is a dummy variable that is \code{1} when
#' \code{fsfi_nzero15 <= 7} (i.e., when the respondent said "no sexual activity"
#' to 7 or fewer of the 15 items with that option), and \code{0} otherwise.  See
#' Baser et al. (2012) for more details on how this cutoff was chosen.
#'
#'
#' @section How Missing Data is Handled:
#' The FSFI authors do not indicate how to handle missing item data when
#' calculating the FSFI scores.  This is unfortunate because women frequently
#' skip items they feel are not relevant to them (e.g., the items asking about
#' satisfaction with "your partner" are often skipped by non-partnered women),
#' leading to an unexpectedly large number of missing subscale and FSFI total
#' scores.  To minimize excessive missing values for the FSFI subscale and Total
#' scores, the \code{fsfi} function handles missing items similarly to the
#' scoring methods for many other PROs.  Specifically, the \code{fsfi} function
#' will calculate the 6 subscale scores as long as at least half of the items on
#' the given subscale have valid, non-missing item responses.  More concretely,
#' each subscale must have at least 2 non-missing responses, except for Desire,
#' which has only 2 items and requires only 1 non-missing response.  The
#' \code{fsfi} function will calculate the FSFI Total Score for a respondent as
#' long as it was able to calculate at least 5 out of the 6 subscale scores.
#' Scores calculated in the presence of missing items are pro-rated so that
#' their theoretical minimum and maximum values are identical those from scores
#' calculated from complete data.
#'
#' These methods of handling missing item responses were chosen to balance the
#' reality that respondents often skip some items with the need to maintain the
#' validity of the scores.  However, I know of no directly applicable empirical
#' study that supports these choices, and I encourage more research into how
#' missing responses affect the psychometrics of this and other instruments.
#'
#' @return A data frame with the following variables is returned:
#'
#' \itemize{
#'   \item \strong{fsfi_des} - FSFI Desire subscale (range 1.2 - 6)
#'   \item \strong{fsfi_arous} - FSFI Arousal subscale (range 0 - 6)
#'   \item \strong{fsfi_lub} - FSFI Lubrication subscale (range 0 - 6)
#'   \item \strong{fsfi_org} - FSFI Orgasm subscale (range 0 - 6)
#'   \item \strong{fsfi_sat} - FSFI Satisfaction subscale (range 0.8 - 6)
#'   \item \strong{fsfi_pain} - FSFI Pain subscale (range 0 - 6)
#'   \item \strong{fsfi_tot} - FSFI Total score (range 2 - 36)
#'   \item \strong{fsfi_dys01} - Indicator of FSFI sexual dysfunction (i.e., of
#'     \code{fsfi_tot <= 26.55}); \code{0} = No Dysfunction, \code{1} =
#'     Dysfunction
#'   \item \strong{fsfi_nzero15} - There are 15 FSFI items that have a response
#'     option of \code{0} ("No sexual activity").  This is the number of those
#'     items with responses of \code{0} or \code{NA} (See Details).
#'   \item \strong{fsfi_sexactive01} - For the FSFI scores to be valid estimates
#'     of sexual functioning, respondents need to have been sexually active
#'     during the 4 week recall period.  This variable indicates whether their
#'     sexual activity levels were high enough for their FSFI scores to be
#'     valid.  Specifically, it is an indicator that \code{fsfi_nzero15 <= 7} (See
#'     Details).
#' }
#'
#' Optionally, the data frame can additionally have variables containing the
#' number of valid item responses on each scale for each respondent (if
#' \code{keepNvalid = TRUE}, but this option might be removed in future package
#' updates).
#'
#' @note
#'   The six FSFI subscale scores are scaled to have a maximum score of 6.0.
#'   The subscale scores are summed to calculate the FSFI Total score, which has
#'   a maximum score of 36.  Because 4 items have no response option scored
#'   \code{0} (2 items from Desire subscale and 2 from Satisfaction subscale),
#'   the minimum possible score for the Desire subscale, the Satisfaction
#'   subscale, and the FSFI Total score is greater than zero.
#'
#' @references
#' Rosen, R, Brown, C, Heiman, J, Leiblum, S, Meston, C, Shabsigh, R, et al.
#' (2000). The Female Sexual Function Index (FSFI): a multidimensional
#' self-report instrument for the assessment of female sexual function.
#' \emph{Journal of Sex & Marital Therapy, 26}(2), 191-208.
#'
#' Wiegel, M, Meston, C, & Rosen, R. (2005). The Female Sexual Function Index
#' (FSFI): Cross-Validation and Development of Clinical Cutoff Scores.
#' \emph{Journal of Sex & Marital Therapy, 31}(1), 1-20.
#'
#' Baser, RE, Li, Y, & Carter, J. (2012). Psychometric validation of the female
#' sexual function index (FSFI) in cancer survivors. \emph{Cancer, 118}(18),
#' 4606-4618.
#'
#' @export
#'
#' @examples
#' # Creating data frame of fake FSFI responses
#' dat <- PROscorerTools::makeFakeData(n = 10, nitems = 19, values = 0:5,
#'                                     prefix = 'f')
#' dat1 <- PROscorerTools::makeFakeData(n = 10, nitems = 4, values = 1:5)
#' names(dat1) <- c('f1', 'f2', 'f15', 'f16')
#' dat[c(1, 2, 15, 16)] <- dat1
#' # Scoring the fake FSFI responses
#' fsfi(dat, 'f')
fsfi <- function(df, iprefix = "fsfi", keepNvalid = FALSE)  {

  itemsfsfi <- df[paste0(iprefix, 1:19)]

  ## Check item ranges:
  if(!PROscorerTools::chk_values(dfItems = itemsfsfi[c(1, 2, 15, 16)],
                values = c(1:5, NA))) {
    stop(paste(strwrap(
      "At least one of your items has a value that is not allowed.
      FSFI items 1, 2, 15, and 16 should have all integer values
      between 1 and 5, or NA (i.e., missing).",
      exdent = 2, width = 65), collapse = "\n"))
  }
  if(!PROscorerTools::chk_values(dfItems = itemsfsfi[c(-1, -2, -15, -16)],
                values = c(0:5, NA))) {
    stop(paste(strwrap(
      "At least one of your items has a value that is not allowed.
      FSFI items 3 through 14, 17, 18, and 19 should have all integer values
      between 0 and 5, or NA (i.e., missing).",
      exdent = 2, width = 65), collapse = "\n"))
  }


# Calculating subscales, requiring 2+ items valid for each scale,
#   except only 1 required for DESIRE
  fsfi_des <- PROscorerTools::scoreScale(df = itemsfsfi, type = "sum",
                                         keepNvalid = TRUE,
                                         items = 1:2,
                                         okmiss = .50,
                                         scalename ="fsfi_des")
  fsfi_arous <- PROscorerTools::scoreScale(df = itemsfsfi, type = "sum",
                                           keepNvalid = TRUE,
                                           items = 3:6,
                                           okmiss = .50,
                                           scalename ="fsfi_arous")
  fsfi_lub <- PROscorerTools::scoreScale(df = itemsfsfi, type = "sum",
                                         keepNvalid = TRUE,
                                        items = 7:10,
                                        okmiss = .50,
                                        scalename ="fsfi_lub")
  fsfi_org <- PROscorerTools::scoreScale(df = itemsfsfi, type = "sum",
                                         keepNvalid = TRUE,
                                         items = 11:13,
                                         okmiss = .50,
                                         scalename ="fsfi_org")
  fsfi_sat <- PROscorerTools::scoreScale(df = itemsfsfi, type = "sum",
                                         keepNvalid = TRUE,
                                         items = 14:16,
                                         okmiss = .50,
                                         scalename ="fsfi_sat")
  fsfi_pain <- PROscorerTools::scoreScale(df = itemsfsfi, type = "sum",
                                          keepNvalid = TRUE,
                                          items = 17:19,
                                          okmiss = .50,
                                          scalename ="fsfi_pain")
  ## Scaling to have max of 6
  fsfi_des[1]   <- .6*fsfi_des[1]
  fsfi_arous[1]  <- .3*fsfi_arous[1]
  fsfi_lub[1] <- .3*fsfi_lub[1]
  fsfi_org[1]   <- .4*fsfi_org[1]
  fsfi_sat[1] <- .4*fsfi_sat[1]
  fsfi_pain[1]     <- .4*fsfi_pain[1]


  scores   <- data.frame(fsfi_des[1], fsfi_arous[1], fsfi_lub[1],
                         fsfi_org[1], fsfi_sat[1], fsfi_pain[1])
  scores_N <- data.frame(fsfi_des[2], fsfi_arous[2], fsfi_lub[2],
                         fsfi_org[2], fsfi_sat[2], fsfi_pain[2])

  ## fsfi_sat: Could have have values < 0.8 (the official floor) if missing an
  ##   item (due to prorating + diff number of response options for the items).
  ##   Fixing to have floor of 0.8.
  scores$fsfi_sat[scores$fsfi_sat < 0.8] <- 0.8


  # FSFI Total Score: Non-missing if at least 5/6 subscales are non-missing
  nmiss_subscales 	  <- rowSums(apply(scores, 2, is.na))
  scores$fsfi_tot 		 	<- rowMeans(scores, na.rm=TRUE)*6
  scores_N$fsfi_tot_N 	<- rowSums(scores_N, na.rm=TRUE)

  # Assigning missing to FSFI Total if missing more than 1 of six subscales.
  scores$fsfi_tot[nmiss_subscales > 1] <- NA
  # Replacing fsfi_tot < 2 with fsfi_tot == 2.0
  # Explanation:
  # fsfi_tot can be < 2.0 (which should be impossible) when fsfi_des or fsfi_sat
  # is missing and the mean of the rest of the scores is prorated to compensate.
  # (fsfi_des and fsfi_sat are the 2 scales that contain the 4 items with no 0
  # response option).
  scores$fsfi_tot[scores$fsfi_tot < 2] <- 2

  # Dysfunction indicators
  scores$fsfi_dys01 <- as.integer(scores$fsfi_tot <= 26.55)
  ### Counting number zero or missing out of 15 items with 0 response option

  flagZeroMiss <- function(x) {as.numeric(is.na(x) | x==0)}
  nZeroMiss <- data.frame(lapply(itemsfsfi, flagZeroMiss))

  scores$fsfi_nzero15 <- rowSums(nZeroMiss[c(-1, -2, -15, -16)])

  ### Sexual Activity variables
  scores$fsfi_sexactive01 <- as.integer(scores$fsfi_nzero15 <= 7)
#  scores$sexActiveFSFI <- NA
#  scores$sexActiveFSFI[sexActive01 == 1] <- "active"
#  scores$sexActiveFSFI[sexActive01 == 0] <- "inactive"

  # scores$fsfi_sexactive01[is.na(scores$fsfi_tot)] <- NA

  if(keepNvalid) {
    out <- data.frame(scores, scores_N)
  } else {
    out <- data.frame(scores)
  }
  return(out)
}
