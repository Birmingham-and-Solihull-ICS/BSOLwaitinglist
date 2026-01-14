#include <Rcpp.h>
using namespace Rcpp;

// Geometric random numbers
IntegerVector rgeom_cpp(int n, double prob) {
    IntegerVector out(n);
    for(int i = 0; i < n; i++) {
        out[i] = R::rgeom(prob);
    }
    return out;
}

//' Join two waiting lists
//' @param wl_1 First waiting list
//' @param wl_2 Second waiting list
//' @param referral_index Integer index of referral column (0-based in C++)
//' @return Joined waiting list
//' @export
// [[Rcpp::export]]
DataFrame wl_join_cpp(DataFrame wl_1, DataFrame wl_2, int referral_index = 0) {
    // Use R's rbind to preserve column classes/attributes, then reorder by referral column
    DataFrame combined = Rcpp::Function("rbind")(wl_1, wl_2);
    CharacterVector col_names = combined.names();
    int n_cols = col_names.size();
    int n_total = combined.nrows();

    // Ensure referral_index within bounds
    if (referral_index < 0 || referral_index >= n_cols) stop("referral_index out of range");

    // Convert string to std::string for DataFrame indexing
    std::string referral_col = std::string(col_names[referral_index]);

    // Compute order (R's order is 1-based)
    IntegerVector order_idx = Rcpp::as<IntegerVector>(Rcpp::Function("order")(combined[referral_col]));

    // Rebuild sorted DataFrame column-by-column (preserve attributes)
    List sorted_cols(n_cols);
    sorted_cols.attr("names") = col_names;
    for (int c = 0; c < n_cols; ++c) {
        SEXP col = combined[c];
        switch (TYPEOF(col)) {
        case REALSXP: {
            NumericVector v(col);
            NumericVector tmp(n_total);
            for (int i = 0; i < n_total; ++i) tmp[i] = v[order_idx[i] - 1];
            // copy attributes (class, levels if present)
            SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
            if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
            SEXP lv = Rf_getAttrib(col, Rf_install("levels"));
            if (!Rf_isNull(lv)) Rf_setAttrib(tmp, Rf_install("levels"), lv);
            sorted_cols[c] = tmp;
            break;
        }
        case INTSXP: {
            IntegerVector v(col);
            IntegerVector tmp(n_total);
            for (int i = 0; i < n_total; ++i) tmp[i] = v[order_idx[i] - 1];
            SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
            if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
            SEXP lv = Rf_getAttrib(col, Rf_install("levels"));
            if (!Rf_isNull(lv)) Rf_setAttrib(tmp, Rf_install("levels"), lv);
            sorted_cols[c] = tmp;
            break;
        }
        case STRSXP: {
            CharacterVector v(col);
            CharacterVector tmp(n_total);
            for (int i = 0; i < n_total; ++i) tmp[i] = v[order_idx[i] - 1];
            SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
            if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
            sorted_cols[c] = tmp;
            break;
        }
        default:
            stop("Unsupported column type in wl_join_cpp; please extend handling for this type");
        }
    }

    DataFrame sorted_df(sorted_cols);
    return sorted_df;
}


//' Schedule a waiting list against a sequence of session dates
//'
//' Applies a capacity schedule to a waiting list and returns a data frame
//' with scheduling results. Column indices are **0-based** (C++ convention).
//'
//' @param waiting_list A `data.frame` containing the waiting list. It should
//'   include date columns referenced by `referral_index` and (optionally) `removal_index`.
//' @param schedule A vector of `Date` values (e.g., `as.Date(...)`) representing
//'   the chronological sequence of session dates used for scheduling.
//' @param referral_index Integer (0-based) column index in `waiting_list` that
//'   holds the referral date. Default is `0`.
//' @param removal_index Integer (0-based) column index in `waiting_list` that
//'   holds the removal or discharge date (if present). Default is `1`.
//' @param unscheduled Logical flag. If `TRUE`, the output will include (or focus on)
//'   cases that could not be scheduled under the provided `schedule`/capacity.
//'
//' @return A `data.frame` with the scheduling results. The precise schema depends
//'   on the implementation, but typically includes scheduled dates and indicators
//'   of whether each record was scheduled or remained unscheduled.
//'
//' @details
//' - Indices are **0-based** in the C++ implementation. When calling from R, pass
//'   integers consistent with the wrapper defaults (e.g., `0L`, `1L`).
//' - `schedule` must be of R's `Date` type; ensure inputs are `as.Date(...)`.
//' - The function assumes `waiting_list` contains the necessary columns referenced
//'   by the supplied indices.
//'
//' @examples
//' \dontrun{
//' wl <- data.frame(
//'   referral_date = as.Date(c("2024-01-10", "2024-01-15", "2024-01-20")),
//'   removal_date  = as.Date(c(NA, "2024-02-05", NA))
//' )
//' sess <- as.Date(c("2024-02-01", "2024-02-08", "2024-02-15"))
//'
//' # Column indices are 0-based in C++ (referral=0, removal=1):
//' res <- wl_schedule_cpp(wl, sess, referral_index = 0L, removal_index = 1L)
//' head(res)
//'
//' # Return/Include unscheduled cases:
//' res_uns <- wl_schedule_cpp(wl, sess, referral_index = 0L, removal_index = 1L,
//'                            unscheduled = TRUE)
//' }
//' @export
// [[Rcpp::export]]
DataFrame wl_schedule_cpp(
        DataFrame waiting_list,
        DateVector schedule,
        int referral_index = 0,
        int removal_index = 1,
        bool unscheduled = false
) {
    int n = waiting_list.nrows();
    if (n == 0) {
        if (!unscheduled) return waiting_list;
    }

    // Work on a copy to preserve original attributes
    List wl_copy = Rcpp::clone(waiting_list);
    DateVector referral = wl_copy[referral_index];
    DateVector removal = wl_copy[removal_index];

    // indices of currently unscheduled (NA removal)
    std::vector<size_t> wl_idx;
    for (size_t i = 0; i < (size_t)n; ++i) {
        if (NumericVector::is_na(removal[i])) wl_idx.push_back(i);
    }

    size_t i = 0;
    IntegerVector scheduled_vec(schedule.size(), 0);
    if (!unscheduled) {
        for (int j = 0; j < schedule.size(); ++j) {
            if (i >= wl_idx.size()) break;
            size_t idx = wl_idx[i];
            if (schedule[j] > referral[idx]) {
                removal[idx] = schedule[j];
                i++;
            }
        }
        // put removal back
        wl_copy[removal_index] = removal;

        // Sort entire updated wl_copy by referral column and return full df
        IntegerVector order_idx = Rcpp::as<IntegerVector>(Rcpp::Function("order")(wl_copy[referral_index]));
        // Reorder columns preserving attributes
        CharacterVector col_names = wl_copy.names();
        int n_cols = col_names.size();
        int rows = referral.size();
        List sorted_cols(n_cols);
        sorted_cols.attr("names") = col_names;
        for (int c = 0; c < n_cols; ++c) {
            SEXP col = wl_copy[c];
            switch (TYPEOF(col)) {
            case REALSXP: {
                NumericVector v(col);
                NumericVector tmp(rows);
                for (int k = 0; k < rows; ++k) tmp[k] = v[order_idx[k] - 1];
                SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
                if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
                SEXP lv = Rf_getAttrib(col, Rf_install("levels"));
                if (!Rf_isNull(lv)) Rf_setAttrib(tmp, Rf_install("levels"), lv);
                sorted_cols[c] = tmp;
                break;
            }
            case INTSXP: {
                IntegerVector v(col);
                IntegerVector tmp(rows);
                for (int k = 0; k < rows; ++k) tmp[k] = v[order_idx[k] - 1];
                SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
                if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
                SEXP lv = Rf_getAttrib(col, Rf_install("levels"));
                if (!Rf_isNull(lv)) Rf_setAttrib(tmp, Rf_install("levels"), lv);
                sorted_cols[c] = tmp;
                break;
            }
            case STRSXP: {
                CharacterVector v(col);
                CharacterVector tmp(rows);
                for (int k = 0; k < rows; ++k) tmp[k] = v[order_idx[k] - 1];
                SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
                if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
                sorted_cols[c] = tmp;
                break;
            }
            default:
                stop("Unsupported column type for sorting in wl_schedule_cpp");
            }
        }
        DataFrame updated_df(sorted_cols);
        return updated_df;
    } else {
        // unscheduled = TRUE: also mark scheduled slots
        for (int j = 0; j < schedule.size(); ++j) {
            if (i >= wl_idx.size()) break;
            size_t idx = wl_idx[i];
            if (schedule[j] > referral[idx]) {
                removal[idx] = schedule[j];
                scheduled_vec[j] = 1;
                i++;
            }
        }

        wl_copy[removal_index] = removal;

        // sort same as above
        IntegerVector order_idx = Rcpp::as<IntegerVector>(Rcpp::Function("order")(wl_copy[referral_index]));
        CharacterVector col_names = wl_copy.names();
        int n_cols = col_names.size();
        int rows = referral.size();
        List sorted_cols(n_cols);
        sorted_cols.attr("names") = col_names;
        for (int c = 0; c < n_cols; ++c) {
            SEXP col = wl_copy[c];
            switch (TYPEOF(col)) {
            case REALSXP: {
                NumericVector v(col);
                NumericVector tmp(rows);
                for (int k = 0; k < rows; ++k) tmp[k] = v[order_idx[k] - 1];
                SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
                if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
                SEXP lv = Rf_getAttrib(col, Rf_install("levels"));
                if (!Rf_isNull(lv)) Rf_setAttrib(tmp, Rf_install("levels"), lv);
                sorted_cols[c] = tmp;
                break;
            }
            case INTSXP: {
                IntegerVector v(col);
                IntegerVector tmp(rows);
                for (int k = 0; k < rows; ++k) tmp[k] = v[order_idx[k] - 1];
                SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
                if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
                SEXP lv = Rf_getAttrib(col, Rf_install("levels"));
                if (!Rf_isNull(lv)) Rf_setAttrib(tmp, Rf_install("levels"), lv);
                sorted_cols[c] = tmp;
                break;
            }
            case STRSXP: {
                CharacterVector v(col);
                CharacterVector tmp(rows);
                for (int k = 0; k < rows; ++k) tmp[k] = v[order_idx[k] - 1];
                SEXP cl = Rf_getAttrib(col, R_ClassSymbol);
                if (!Rf_isNull(cl)) Rf_setAttrib(tmp, R_ClassSymbol, cl);
                sorted_cols[c] = tmp;
                break;
            }
            default:
                stop("Unsupported column type for sorting in wl_schedule_cpp");
            }
        }
        DataFrame updated_df(sorted_cols);
        return List::create(_["updated_list"] = updated_df, _["scheduled"] = scheduled_vec);
    }
}


//' Waiting list simulator
//'
//' Simulates waiting list dynamics between optional start/end dates given
//' arrival (demand), capacity, and optional withdrawals. An existing waiting
//' list can be supplied to seed the initial state. If `detailed_sim = TRUE`,
//' the function returns a more granular trajectory.
//'
//' @param start_date_ `Date` or `NULL`. Start date of the simulation.
//'   If `NULL`, an internal default is used.
//' @param end_date_ `Date` or `NULL`. End date of the simulation.
//'   If `NULL`, an internal default is used.
//' @param demand Numeric, expected arrivals per time unit (e.g., per day or per session).
//'   Default `10.0`.
//' @param capacity Numeric, expected capacity per time unit (e.g., per day or per session).
//'   Default `11.0`.
//' @param waiting_list_ `data.frame` or `NULL`. Optional initial waiting list to seed
//'   the simulation. If `NULL`, the simulation starts from an empty list or internal default.
//' @param withdrawal_prob Numeric probability in `[0, 1]` for withdrawal events,
//'   or `NA_real_` to disable withdrawals. Default `NA_real_`.
//' @param detailed_sim Logical. If `TRUE`, return detailed per-period results;
//'   otherwise return an aggregated summary. Default `FALSE`.
//'
//' @return A `data.frame` with the simulated waiting list trajectory. The schema depends
//'   on `detailed_sim` (e.g., per-period metrics when `TRUE`, aggregated endpoints when `FALSE`).
//'
//' @details
//' - `start_date_` / `end_date_` are `Nullable<Date>` at the C++ level; pass `NULL`
//'   from R if you want internal defaults.
//' - `waiting_list_` may be `NULL` or a `data.frame` with necessary columns as expected
//'   by the simulator implementation.
//' - `withdrawal_prob = NA_real_` disables withdrawals; otherwise specify a probability
//'   between 0 and 1.
//'
//' @examples
//' \dontrun{
//' # Simple run with explicit dates and parameters
//' res <- wl_simulator_cpp(
//'   start_date_   = as.Date("2024-01-01"),
//'   end_date_     = as.Date("2024-03-31"),
//'   demand        = 12.0,
//'   capacity      = 10.0,
//'   waiting_list_ = NULL,
//'   withdrawal_prob = NA_real_,
//'   detailed_sim  = TRUE
//' )
//' head(res)
//'
//' # Seed with an initial waiting list and enable withdrawals
//' wl0 <- data.frame(
//'   referral_date = as.Date(c("2023-12-10", "2023-12-20")),
//'   priority      = c(1L, 2L)
//' )
//' res2 <- wl_simulator_cpp(
//'   start_date_   = as.Date("2024-01-01"),
//'   end_date_     = as.Date("2024-02-01"),
//'   demand        = 8.0,
//'   capacity      = 9.0,
//'   waiting_list_ = wl0,
//'   withdrawal_prob = 0.05,
//'   detailed_sim  = FALSE
//' )
//' }
//' @export
// [[Rcpp::export]]
DataFrame wl_simulator_cpp(
        Nullable<Date> start_date_ = R_NilValue,
        Nullable<Date> end_date_ = R_NilValue,
        double demand = 10.0,
        double capacity = 11.0,
        Nullable<DataFrame> waiting_list_ = R_NilValue,
        double withdrawal_prob = NA_REAL,
        bool detailed_sim = false
) {
    // Start and end dates
    Date start_date = start_date_.isNotNull() ? as<Date>(start_date_) : as<Date>(Rcpp::Function("Sys.Date")());
    Date end_date = end_date_.isNotNull() ? as<Date>(end_date_) : start_date + 31;

    // Handle waiting_list (default NULL becomes empty DataFrame)
    DataFrame waiting_list = waiting_list_.isNotNull() ? as<DataFrame>(waiting_list_) : DataFrame::create();

    int number_of_days = end_date - start_date;
    double total_demand = demand * number_of_days / 7.0;
    double daily_capacity = capacity / 7.0;

    // Realized demand and referral dates using sample() equivalent
    int realized_demand = R::rpois(total_demand);
    DateVector referral(realized_demand);

    // Match R's sample() behavior: sample dates with replacement
    IntegerVector day_offsets = Rcpp::as<IntegerVector>(
        Rcpp::Function("sample")(
                Rcpp::Function("seq")(0, number_of_days, 1),
                realized_demand,
                Rcpp::Named("replace") = true
        )
    );

    for(int i = 0; i < realized_demand; i++) {
        referral[i] = start_date + day_offsets[i];
    }
    std::sort(referral.begin(), referral.end());

    // Initialize removal and withdrawal
    DateVector removal(realized_demand);
    DateVector withdrawal(realized_demand);
    std::fill(removal.begin(), removal.end(), Date(NumericVector::get_na()));
    std::fill(withdrawal.begin(), withdrawal.end(), Date(NumericVector::get_na()));

    // Withdrawals (computed but only used if withdrawal_prob is not NA)
    if(!NumericVector::is_na(withdrawal_prob)) {
        IntegerVector geom_draw = rgeom_cpp(realized_demand, withdrawal_prob);
        for(int i = 0; i < realized_demand; i++) {
            Date w = referral[i] + geom_draw[i] + 1;
            if(w > end_date) w = Date(NumericVector::get_na());
            withdrawal[i] = w;
        }
    }

    // Construct new waiting list
    // Only include Withdrawal column if withdrawal_prob is not NA in the same way R does
    DataFrame wl_simulated;
    if (NumericVector::is_na(withdrawal_prob)) {
        wl_simulated = DataFrame::create(
            _["Referral"] = referral,
            _["Removal"] = removal
        );
    } else {
        wl_simulated = DataFrame::create(
            _["Referral"] = referral,
            _["Removal"] = removal,
            _["Withdrawal"] = withdrawal
        );
    }

    // Merge with existing waiting_list if provided
    if (waiting_list.nrows() > 0) {
        wl_simulated = wl_join_cpp(waiting_list, wl_simulated);
    }

    // Schedule patients
    if (daily_capacity > 0) {
        // Follow R logic: offsets <- ceiling(seq(0, number_of_days - 1, by = 1 / daily_capacity))
        if (number_of_days > 0) {
            int total_slots = static_cast<int>(std::floor(number_of_days * daily_capacity)) + 1;
            if (total_slots < 0) total_slots = 0;
            DateVector schedule(total_slots);
            double step = 1.0 / daily_capacity;
            for (int k = 0; k < total_slots; ++k) {
                int offset = static_cast<int>(std::ceil(k * step)); // -> 0..number_of_days
                if (offset > number_of_days) offset = number_of_days; // safe clamp
                schedule[k] = start_date + offset;
            }


            // Call wl_schedule_cpp to update the full waiting list (it now preserves all columns)
            wl_simulated = wl_schedule_cpp(wl_simulated, schedule);
        }
    }

    return wl_simulated;
}
