#' Monte Carlo simulation of waiting list
#'
#' A function, designed to be called from a loop, map or replicate, that allows repeated waiting list simulations,
#' passing the run_id and returning the daily waiting list in each run.  Process is stochastic, and designed to be different, using
#' random numbers drawn from distribution of your referrals and removals. This can then be averaged over for output.
#'
#' @param .data Data.frame.  Control data containing summarised data periods start and end dates,
#'  'demand/referrals' ('adds'), 'capacity/removals' ('removes').
#' @param seed Numeric.   Specify a seed for reproducible outputs
#' @param start_date_name Character.  Name of the column containing start date.
#' @param end_date_name Character.  Name of the column containing end date.
#' @param adds_name Character.  Name of the column containing number of referrals/additions/demand.
#' @param removes_name Character.  Name of the column containing number of removals/capacity.
#' @param starting_wl Numeric.  The current waiting list size at the start of the period.
#' @param run_id When using in Monte Carlo, this would be 'i' for each replication. Default is 1 to run once.
#' @param force_boundary_alignment If TRUE, set the last day of each period’s size to the actual carry-over.
#' @param burn_in_days Number of days burn-in for initial backlog dispersion. Default is 90.
#'
#' @returns A data.frame of dates (days) with waiting list size at each date.
#'
#' @export
#'
#' @examples
#'  # Monte Carlo control data
#'  control_data <-
#'   data.frame(
#'   start_date = as.Date(c("2024-04-01", "2024-07-01"), "%Y-%m-%d"),
#'   end_date = as.Date(c("2024-06-30", "2024-09-30"), "%Y-%m-%d"),
#'   Referrals = c(85, 112),
#'   Removals = c(90, 95)
#'   )
#'
#'   sim_wls <-
#'      bsol_montecarlo_WL3(
#'          .data = control_data,
#'          start_date_name = "start_date",
#'          end_date_name = "end_date",
#'          adds_name = "Referrals",
#'          removes_name = "Removals",
#'          starting_wl = 450
#'      )
#'
bsol_montecarlo_WL3 <-
    function(.data, seed,
             start_date_name = "start_date",
             end_date_name   = "end_date",
             adds_name       = "added",    # <-- set to "Referrals" for your data
             removes_name    = "removed",  # <-- set to "Removals"  for your data
             starting_wl     = 0L,
             run_id          = 1L,
             # If TRUE, set the last day of each period’s size to the actual carry-over
             force_boundary_alignment = TRUE,
             # Burn-in window for initial backlog dispersion
             burn_in_days = 90L) {

        # -- Ensure date columns are Date --
        if (!inherits(.data[[start_date_name]], "Date")) {
            .data[[start_date_name]] <- as.Date(.data[[start_date_name]])
        }
        if (!inherits(.data[[end_date_name]], "Date")) {
            .data[[end_date_name]] <- as.Date(.data[[end_date_name]])
        }

        sim_period_n <- nrow(.data)

        # === A. Seed only once ===
        if (!missing(seed)) set.seed(seed)

        sims       <- vector("list", sim_period_n)
        sizes_list <- vector("list", sim_period_n)

        # --- Robust column pickers based on types (no name assumptions) ---
        get_date_col_index <- function(df) {
            idx <- which(vapply(df, function(x) inherits(x, "Date"), logical(1)))
            if (length(idx) == 0L) stop("wl_queue_size output: no Date-like column found.")
            idx[1]
        }
        get_size_col_index <- function(df, date_idx) {
            idx <- which(vapply(df, is.numeric, logical(1)))
            idx <- setdiff(idx, date_idx)  # exclude the Date column if numeric under the hood
            if (length(idx) == 0L) stop("wl_queue_size output: no numeric size column found.")
            idx[1]
        }

        for (i in seq_len(sim_period_n)) {
            start_i   <- .data[[start_date_name]][i]
            end_i     <- .data[[end_date_name]][i]
            adds_i    <- as.integer(.data[[adds_name]][i])
            removes_i <- as.integer(.data[[removes_name]][i])

            if (i == 1L) {
                # === C. Spread initial backlog over a pre-period window (burn-in) ===
                if (starting_wl > 0L) {
                    base_dates <- seq.Date(from = start_i - burn_in_days, to = start_i - 1L, by = "day")
                    current_wl <- data.frame(
                        Referral = sample(base_dates, starting_wl, replace = TRUE),
                        Removal  = as.Date(NA)
                    )
                    sims[[i]] <- wl_simulator_cpp(start_i, end_i, adds_i, removes_i, waiting_list = current_wl)
                } else {
                    sims[[i]] <- wl_simulator_cpp(start_i, end_i, adds_i, removes_i)
                }

                # Queue sizes for period i, trimmed to [start_i, end_i] (removes burn-in from output)
                sizes_i <- NHSRwaitinglist::wl_queue_size(sims[[i]])
                d_idx   <- get_date_col_index(sizes_i)
                s_idx   <- get_size_col_index(sizes_i, d_idx)
                in_rng  <- sizes_i[[d_idx]] >= start_i & sizes_i[[d_idx]] <= end_i
                sizes_i <- sizes_i[in_rng, , drop = FALSE]
                sizes_list[[i]] <- sizes_i

            } else {
                # Previous period simulation result
                prev <- sims[[i - 1L]]
                stopifnot(is.data.frame(prev), all(c("Referral","Removal") %in% names(prev)))

                # === B. Carry forward ONLY unresolved cases as input to next period ===
                carry_over <- prev[is.na(prev$Removal), c("Referral","Removal"), drop = FALSE]

                # Optional: align the end-of-period size to carry-over to visually avoid a boundary jump
                if (force_boundary_alignment) {
                    prev_sizes <- sizes_list[[i - 1L]]
                    if (!is.null(prev_sizes) && nrow(prev_sizes) > 0L) {
                        pd_idx <- get_date_col_index(prev_sizes)
                        ps_idx <- get_size_col_index(prev_sizes, pd_idx)
                        prev_sizes[nrow(prev_sizes), ps_idx] <- nrow(carry_over)
                        sizes_list[[i - 1L]] <- prev_sizes
                    }
                }

                # Simulate current period with carried backlog
                sims[[i]] <- wl_simulator_cpp(start_i, end_i, adds_i, removes_i, waiting_list = carry_over)

                # Queue sizes for current period, trimmed to [start_i, end_i]
                sizes_i <- NHSRwaitinglist::wl_queue_size(sims[[i]])
                d_idx   <- get_date_col_index(sizes_i)
                s_idx   <- get_size_col_index(sizes_i, d_idx)
                in_rng  <- sizes_i[[d_idx]] >= start_i & sizes_i[[d_idx]] <= end_i
                sizes_i <- sizes_i[in_rng, , drop = FALSE]
                sizes_list[[i]] <- sizes_i
            }
        }

        # Build continuous series by binding all trimmed period outputs.
        # (Boundary day is retained; if alignment is on, last day equals next carry-over.)
        sizes_all <- do.call(rbind, sizes_list)
        sizes_all$run_id <- run_id
        return(sizes_all)
    }
