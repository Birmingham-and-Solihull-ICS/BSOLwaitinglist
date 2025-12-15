#' Mean wait required for a desired proportion exceeding target
#'
#' For exponential waiting times, returns the mean wait Wbar such that
#' P(W > T) = p_exceed. Uses Wbar = T / ln(1 / p_exceed).
#'
#' @param threshold numeric (>0). The waiting time threshold T (e.g., weeks).
#' @param p_exceed numeric in (0,1). Desired proportion exceeding T.
#' @return numeric. The mean wait Wbar.
#' @examples
#' # P4 example: target T = 52 weeks
#' mean_wait_for_exceeding_target(52, p_exceed = 0.018)  # ~12.94 weeks (≈ a quarter of target)
#' mean_wait_for_exceeding_target(52, p_exceed = 0.002)  # ~8.37 weeks (≈ a sixth of target)
#' mean_wait_for_exceeding_target(52, p_exceed = 0.05)   # ~17.35 weeks
#'
#' # Vectorised input:
#' mean_wait_for_exceeding_target(52, p_exceed = c(0.10, 0.05, 0.018))
mean_wait_for_exceeding_target <- function(threshold, p_exceed) {
    if (any(!is.finite(threshold)) || any(threshold <= 0))
        stop("`threshold` must be a finite, positive numeric value (or vector).")
    if (any(!is.finite(p_exceed)) || any(p_exceed <= 0 | p_exceed >= 1))
        stop("`p_exceed` must be a finite numeric in the open interval (0, 1).")
    threshold / log(1 / p_exceed)  # log() is natural log in R
}

mean_wait_for_exceeding_target(52, p_exceed = 0.018)


#' Mean wait required for a desired proportion meeting target
#'
#' For exponential waiting times, returns the mean wait Wbar such that
#' P(W > T) = p_exceed. Uses Wbar = T / ln(1 / (1-p_meeting)).
#'
#' @param threshold numeric (>0). The waiting time threshold T (e.g., weeks).
#' @param p_meeting numeric in (0,1). Desired proportion meeting T.
#' @return numeric. The mean wait Wbar.
#' @examples
#' # P4 example: target T = 52 weeks
#' mean_wait_for_meeting_target(52, p_exceed = 0.982)  # ~12.94 weeks (≈ a quarter of target)
#' mean_wait_for_meeting_target(52, p_exceed = 0.998)  # ~8.37 weeks (≈ a sixth of target)
#' mean_wait_for_meeting_target(52, p_exceed = 0.95)   # ~17.35 weeks
#'
#' # Vectorised input:
#' mean_wait_for_meeting_target(52, p_meeting = c(0.982, 0.998, 0.95))
mean_wait_for_meeting_target <- function(threshold, p_meeting) {
    if (any(!is.finite(threshold)) || any(threshold <= 0))
        stop("`threshold` must be a finite, positive numeric value (or vector).")
    if (any(!is.finite(p_meeting)) || any(p_meeting <= 0 | p_meeting >= 1))
        stop("`p_meeting` must be a finite numeric in the open interval (0, 1).")
    threshold / log(1 / (1 - p_meeting))  # log() is natural log in R
}



#' Calculate the waiting times target from given mean wait and proportion exceeding
#'
#' Given mean wait and desired proportion exceeding, return the threshold T
#' using T = - Wbar * ln(p_exceed).
#'
#' @param mean_wait Mean waiting time (Wbar)
#' @param p_exceed numeric in (0,1). Desired proportion exceeding T.
#'
#' @returns numeric. The target waiting time (T)
#' @export
#'
#' @examples
#' # P4 example: target should be T = 52 weeks
#' threshold_for_exceeding(12.943748, 0.018)
#' threshold_for_exceeding(8.367382, 0.002)
#' threshold_for_exceeding(17.358026, 0.05)
#'
#'
threshold_for_exceeding <- function(mean_wait, p_exceed) {
    if (any(!is.finite(mean_wait)) || any(mean_wait <= 0))
        stop("`mean_wait` must be a finite, positive numeric value.")
    if (any(!is.finite(p_exceed)) || any(p_exceed <= 0 | p_exceed >= 1))
        stop("`p_exceed` must be in (0, 1).")
    - mean_wait * log(p_exceed)
}



#' Time threshold taken
#'
#' Given mean wait and desired proportion meting target, return the threshold T
#' using T = - Wbar * ln(1-p_meeting).
#'
#' @param mean_wait Mean waiting time (Wbar)
#' @param p_meeting numeric in (0,1). Desired proportion meeting T.
#'
#' @returns numeric. The target waiting time (T)
#' @export
#'
#' @examples
#' # P4 example: target should be T = 52 weeks
#' threshold_for_meeting(12.943748, 0.982)
#' threshold_for_meeting(8.367382, 0.998)
#' threshold_for_meeting(17.358026, 0.95)
#'
threshold_for_meeting <- function(mean_wait, p_meeting) {
    if (any(!is.finite(mean_wait)) || any(mean_wait <= 0))
        stop("`mean_wait` must be a finite, positive numeric value.")
    if (any(!is.finite(p_meeting)) || any(p_meeting <= 0 | p_meeting >= 1))
        stop("`p_exceed` must be in (0, 1).")
    - mean_wait * log(1 - p_meeting)
}


#' Proportion exceeding target, given target and mean wait
#'
#' Given mean wait and threshold, return proportion exceeding target p = P(W > T)
#' using p = exp(- T / Wbar).
#'
#' @param mean_wait Mean waiting time (Wbar)
#' @param threshold numeric (>0). The waiting time threshold T (e.g., weeks).
#'
#' @returns Numeric.
#' @export
#'
#' @examples
#' exceeding_prob(12.94375, threshold = 52)
exceeding_prob <- function(mean_wait, threshold) {
    if (any(!is.finite(mean_wait)) || any(mean_wait <= 0))
        stop("`mean_wait` must be a finite, positive numeric value.")
    if (any(!is.finite(threshold)) || any(threshold <= 0))
        stop("`threshold` must be a finite, positive numeric value.")
    exp(-threshold / mean_wait)
}







#' Calculate factor argument, based on proportion exceeding target
#'
#'
#' Numeric factor used in average wait calculation - to get a quarter of
#' the target use factor=4 and one sixth of the target use factor = 6 etc. Defaults to 4.
#' see NHSRwaiting list
#'
#' The "factor" k = T / mean = ln(1/p)
#'
#' @param p_exceed numeric in (0,1). Desired proportion exceeding T.
#'
#' @returns Numeric. Factor (k)
#' @export
#'
#' @examples
#' factor_for_exceeding(0.018)
#' factor_for_exceeding(0.002)
#' factor_for_exceeding(0.05)
factor_for_exceeding <- function(p_exceed) {
    if (any(!is.finite(p_exceed)) || any(p_exceed <= 0 | p_exceed >= 1)) stop("p_exceed in (0,1)")
    log(1 / p_exceed)
}



#' Calculate the "factor", by proportion meeting target
#'
#' Numeric factor used in average wait calculation - to get a quarter of
#' the target use factor=4 and one sixth of the target use factor = 6 etc. Defaults to 4.
#' see NHSRwaiting list
#'
#' The "factor" k = T / mean = ln(1/p)
#'
#' @param p_meeting numeric in (0,1). Desired proportion meeting T.
#'
#' @returns Numeric. The 'factor' value (k)
#' @export
#'
#' @examples
#' factor_for_meeting(0.982)
#' factor_for_meeting(0.998)
#' factor_for_meeting(0.95)
factor_for_meeting <- function(p_meeting) {
    if (any(!is.finite(p_meeting)) || any(p_meeting <= 0 | p_meeting >= 1)) stop("p_exceed in (0,1)")
    log(1 / (1 - p_meeting))
}



#' Mean wait as a proporiton of target wait.
#'
#' Calculates the mean waiting time, based on exponential distribution, as a
#' fraction of target wait (T) = 1 / k
#'
#' @param p_exceed numeric in (0,1). Desired proportion exceeding T.
#'
#' @returns Numeric.  Mean as a fraction of the target (1/k)
#' @export
#'
#' @examples
#' mean_fraction_of_target(0.018) # 0.2489182
#' # So, applying to 52 weeks, mean wait for 0.018 exceeding (98.2 meeting)
#' # = 52 * 0.2489182
mean_fraction_of_target <- function(p_exceed) {
    1 / factor_for_exceeding(p_exceed)
}




