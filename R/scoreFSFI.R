#####====================================================================================#####
#  Program: 				FSFI Scoring Syntax
#  Author: 					Ray Baser
#  Original Date Written: 	2016-08-26
#  Original Purpose: 		PROscorer
#####====================================================================================#####


#####  Steps -------
#   - Make indicators of missing for each item
#		- Sum total miss for all items
#		- Sum total miss for each subscale
#   - Make indicators of zero response for the 15 relevant items
#		- Sum total zero for all items
#		- Sum total zero for each subscale
#	- Make indicators of missing OR zero for each item
#		- Sum total miss/zero for all items
#		- Sum total miss/zero for each subscale
#
#	- Create sexActiveFSFI variable based on total missing/zero <= 7
#
#	- Score subscales requiring at least 2 valid per subscale
#		- Score Total Score requiring 5 out of 6 valid subscale scores
#		- Create fsfiSexDys variable based on 25 (or was it 26?) and one based on 25.55 (or whatever it is)

########## 			*** BELOW NOT DONE AS OF 2013-08-13 ***			##########

#	- Score subscales requiring only 1 valid per subscale,
#		- Total Score still requires 5 out of 6 valid subscale scores
#		- Create same fsfiSexDys variables as above, based on this Total Score version
#
#	- EXPERIMENTAL ONLY, DO IF TIME:
#		- Score similarly to above, but recode zeros as missing



###  "items" is a N*19 matrix or data frame of FSFI item responses, IN ORDER.
###	 The names of the individual items shouldn't matter.

#' @title Score the Female Sexual Function Index (FSFI)
#'
#' @description Scores the Female Sexual Function Index (FSFI)
#'
#' @param df A data frame containing responses to the 19 FSFI items, and
#'   possibly other variables.
#' @param iprefix Item number prefix.  Quote the letter(s) preceding the FSFI
#'   item numbers in your data frame.  If this argument is omitted, the function
#'   will assume that your items are named "fsfi1", "fsfi2", etc.
#' @param keepNvalid Logical, whether to return variables containing the
#'   number of valid, non-missing items on each scale for each respondent should
#'   be returned in the data frame with the scale scores.  The default is
#'   \code{FALSE}.  Set to \code{TRUE} to return these variables, which will be
#'   named \code{"scalename_N"} (e.g., \code{fsfi_pain_N}).
#'
#'
#' @return A data frame with the following scale scores is returned:
#'
#' \itemize{
#'   \item \strong{fsfi_des} - FSFI Desire subscale (range 2 - 6)
#'   \item \strong{fsfi_arous} - FSFI Arousal subscale
#'   \item \strong{fsfi_lub} - FSFI Lubrication subscale
#'   \item \strong{fsfi_org} - FSFI Orgasm subscale
#'   \item \strong{fsfi_sat} - FSFI Satisfaction subscale
#'   \item \strong{fsfi_pain} - FSFI Pain subscale
#'   \item \strong{fsfi_tot} - FSFI Total score
#'   \item \strong{fsfi_dys01} - Indicator of FSFI sexual dysfunction (i.e.,
#'     fsfi_tot <= 26.55)
#'   \item \strong{fsfi_nzero15} - There are 15 FSFI items that have a response
#'     option of \code{0} ("No sexual activity").  This is the number of those
#'     items with responses of 0 or NA. (See Details.)
#'   \item \strong{fsfi_sexactive01} - For the FSFI scores to be valid estimates
#'     of sexual functioning, respondents need to have been sexually active
#'     during the 4 week recall period.  This variable indicates whether their
#'     sexual activity levels were high enough for their FSFI scores to be
#'     valid. Specifically, it is an indicator that fsfi_nzero15 <= 7.  (See
#'     Details.)
#' }
#'
#'   Optionally, the data frame can additionally have variables containing the
#'   number of valid item responses on each scale for each respondent.
#'
#' @export
#'
#' @examples
#' dat <- PROscorerTools::makeFakeData(n = 10, nitems = 19, values = 0:5)
#' missFix <- function(df, missvals) {
#'   fixer <- function(x) {
#'     x[x %in% missvals] <- NA
#'     x
#'   }
#'   df[] <- lapply(df, fixer )
#'   df
#' }
#' dat[c(1, 2, 15, 16)] <- missFix(dat[c(1, 2, 15, 16)], 0)
#' names(dat) <- paste0("f", 1:19)
#' scoreFSFI(dat, "f")
scoreFSFI <- function(df, iprefix = "fsfi", keepNvalid = FALSE)  {

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
