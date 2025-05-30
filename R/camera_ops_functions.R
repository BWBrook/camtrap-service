# camera_ops_functions.R -------------------------------------------------
# All heavy lifting lives here.  Import the needed bits explicitly.

import::from("dplyr", `%>%`, arrange, mutate, lag, group_by, ungroup, filter, 
             last,group_modify, select, summarise, n, first, last, across, 
             left_join, everything, case_when, lead, reframe, rowwise, if_else, 
             row_number)
import::from("tidyr", complete, pivot_wider, replace_na)
import::from("lubridate", parse_date_time, as_date, days)
import::from("purrr", map2)
import:::from("zoo", na.locf)

# ---------- PARAMETERS (tweak here) ------------------------------------
SERVICE_BOUNCE  <- 500   # mV jump that signals fresh batteries
# -----------------------------------------------------------------------

#' Parse raw camera-trap records to canonical types
#'
#' Converts timestamp strings to POSIXct (UTC), adds a `date` column
#' (calendar day), and coerces battery (`bl`) and counter fields (`rd`,`li`)
#' to numeric.  Performs *no* filtering or unit logic.
#'
#' @param dat A data frame with columns `datetime`, `bl`, `rd`, `li`.
#'
#' @return The same data frame with parsed columns.
#' @export
parse_columns <- function(dat) {
  dat %>%
    mutate(
      datetime = parse_date_time(datetime, orders = 
                                   c("dmy_HM", "dmy_HMS", "ymd_HMS"), tz = "UTC"),
      date     = as_date(datetime),
      bl       = as.numeric(bl),
      rd       = as.numeric(rd),
      li       = as.numeric(li)
    )
}

#' Identify distinct camera units within each site
#'
#' Uses discontinuities in lifetime-image (`li`) or run-day (`rd`) counters
#' to infer when a physical camera was swapped or factory-reset.  Adds
#' a `cam_id` column of the form `"SITE_u1"`, `"SITE_u2"`, … .
#'
#' @param df Data after \code{parse_columns()} (must contain `site`,
#'   `datetime`, `li`, and `rd`).
#'
#' @return Input rows plus the new `cam_id` column.
#' @export
tag_units <- function(df) {
  df %>% 
    arrange(site, datetime) %>%
    group_by(site) %>%
    mutate(
      li_diff = li - lag(li),
      rd_diff = rd - lag(rd),
      unit_switch = case_when(
        is.na(li_diff) ~ 0L,
        li_diff < 0 | li_diff > 10 ~ 1L,   # LI should +1 exactly
        rd_diff < 0                ~ 1L,   # RD never decreases
        TRUE                       ~ 0L
      ),
      cam_id = paste0(site, "_u", cumsum(unit_switch) + 1L)
    ) %>% 
    ungroup() %>% 
    select(-li_diff, -rd_diff, -unit_switch)
}

#' Daily aggregation (images + median battery)
#'
#' Collapses to **one row per camera-day** with image count and median mV.
#'
#' @param dat_with_units Data containing `cam_id`, `date`, `bl`.
#'
#' @return A data frame with columns `cam_id`, `date`, `count`, `bl`.
#' @export
daily_aggregate <- function(dat_with_units) {
  dat_with_units %>%
    group_by(cam_id, date) %>%
    summarise(
      count = n(),
      bl    = median(bl, na.rm = TRUE),
      rd    = dplyr::last(rd[order(datetime)]),
      li    = dplyr::last(li[order(datetime)]),
      .groups = "drop"
    )
}

#' Fill missing days for every camera unit
#'
#' Ensures a dense, gap-free daily series between the first and last
#' observation of each `cam_id`.
#'
#' @param daily Output of \code{daily_aggregate()}.
#'
#' @return Same columns as input; silent days have `count = 0`, `bl = NA`.
#' @export
expand_daily_series <- function(daily) {
  daily %>%
    group_by(cam_id) %>%
    complete(
      date = seq(min(date), max(date), by = "1 day"),
      fill = list(count = 0, bl = NA_real_)
    ) %>%
    ungroup()
}

#' Detect battery-service events for camera units
#'
#' Flags a new `service` whenever battery voltage rebounds by
#' \code{SERVICE_BOUNCE} mV or at the first record of each unit, and
#' assigns cumulative `service_id`.
#'
#' @param daily_filled Dense daily series (output of
#'   \code{expand_daily_series()}).
#'
#' @return Data frame with added `service`, `service_id`, `bl_jump`.
#' @export
detect_services <- function(daily_filled) {
  daily_filled %>%
    group_by(cam_id) %>%
    group_modify(~ {
      .x %>%
        arrange(date) %>%
        mutate(
          bl_prev = na.locf(lag(bl), na.rm = FALSE),
          bl_jump = bl - bl_prev,
          service = (bl_jump >= 500)
        ) %>%
        replace_na(list(service = FALSE)) %>%
        mutate(service = replace(service, 1, TRUE),
               service_id = cumsum(service)) %>%
        select(-bl_prev)
    }) %>%
    ungroup()
}

#' Flag operational days for **all** camera units
#'
#' Within each service period the camera is considered operational
#' (`oper = 1`) from the first to the last day that captured an image;
#' all days between are filled as alive.
#'
#' @param daily_with_services Output of \code{detect_services()}.
#'
#' @return Data frame with an `oper` column (0/1).
#' @export
flag_operational <- function(daily_with_services) {
  daily_with_services %>%
    group_by(cam_id) %>%
    group_modify(~ {
      .x %>%
        group_by(service_id) %>%
        mutate(
          first_img = min(date[count > 0], na.rm = TRUE),
          last_img  = max(date[count > 0], na.rm = TRUE),
          oper      = as.integer(date >= first_img & date <= last_img)
        ) %>%
        ungroup() %>%
        select(-first_img, -last_img)
    }) %>%
    ungroup()
}

#' Attach site name to each record
#'
#' Extracts the substring before `\"_u\"` in `cam_id` and stores it as `site`.
#'
#' @param daily2 Data containing `cam_id`.
#'
#' @return Same rows plus `site`.
#' @export
add_site_column <- function(daily2) {
  daily2 %>%
    mutate(site = sub("_u.*", "", cam_id))
}

#' Zero anomalous gaps and split a new service at first image after the gap
#'
#' * Works per site so gap thresholds reflect local activity.
#' * Within each camera unit (`cam_id`) any gap whose length exceeds the
#'   site-level quantile `q` is set `oper = 0`; the first non-gap row
#'   thereafter gets `service = TRUE` and all subsequent rows get an
#'   incremented `service_id`.
#'
#' @param daily2   data after flag_operational_days()  (cols: site, cam_id,
#'                 date, count, service, service_id, oper, bl …)
#' @param q        quantile defining a “too long” gap (default 0.95)
#' @return         daily2 with oper/service/service_id updated
zero_anomalous_gaps <- function(daily2, q = 0.95) {
  # ------------------------------------------------ helper for one camera
  patch_cam <- function(df_cam, gap_thresh) {
    is_gap  <- (df_cam$count == 0 & df_cam$oper == 1)
    # rle over full series
    rle_gap <- rle(is_gap)
    idx     <- 1
    new_service <- rep(FALSE, nrow(df_cam))
    for (i in seq_along(rle_gap$lengths)) {
      span_len <- rle_gap$lengths[i]
      span_end <- idx + span_len - 1
      if (rle_gap$values[i] && span_len > gap_thresh) {
        # mark gap as non-operational
        df_cam$oper[idx:span_end] <- 0L
        # first row AFTER the gap gets a new service flag
        wake <- span_end + 1
        if (wake <= nrow(df_cam) && df_cam$count[wake] > 0) {
          new_service[wake] <- TRUE
        }
      }
      idx <- span_end + 1
    }
    # rebuild service flags & ids
    df_cam$service <- df_cam$service | new_service
    df_cam$service_id <- cumsum(df_cam$service)
    df_cam
  }
  
  # ------------------------------------------------ helper for one site
  patch_site <- function(df_site) {
    # gap length distribution (site level)
    gap_lengths <- with(df_site,
                        rle(count == 0 & oper == 1)$lengths[
                          rle(count == 0 & oper == 1)$values])
    thr <- max(6, if (length(gap_lengths)) quantile(gap_lengths, probs = q) else Inf)
    
    df_site %>%
      group_by(cam_id) %>%
      group_modify(~ patch_cam(.x, thr)) %>%
      ungroup()
  }
  
  daily2 %>%
    arrange(site, cam_id, date) %>%
    group_by(site) %>%
    group_modify(~ patch_site(.x)) %>%
    ungroup()
}

#' Summarise service-period statistics for every camera unit
#'
#' Calculates duration, image count, start/end battery medians, and min
#' battery for each `service_id`.
#'
#' @param daily2 Data after gap-handling (must include `oper`, `service_id`,
#'   `bl`, `count`).
#'
#' @return One row per camera-service.
#' @export
summarise_services <- function(daily2) {
  daily2 %>%
    group_by(cam_id, service_id) %>%
    summarise(
      start    = min(date[oper == 1], na.rm = TRUE),
      end      = max(date[oper == 1], na.rm = TRUE),
      days     = as.integer(end - start) + 1L,
      images   = sum(count),
      bl_start = as.integer(round(median(bl[order(date)][1:min(5, sum(!is.na(bl)))], na.rm = TRUE))),
      bl_end   = as.integer(round(median(
        rev(bl[!is.na(bl)])[1:min(5, sum(!is.na(bl)))]
        , na.rm = TRUE))),
      min_bl   = as.integer(round(min(bl, na.rm = TRUE))),
      lt_days  = dplyr::last(rd[order(date)]),
      lt_images= dplyr::last(li[order(date)]),
      .groups  = "drop"
    )
}

#' Attach unit-level uptime (%), penalising downtime until replacement
#'
#' Computes uptime as the ratio of days the unit was alive to the total
#' responsibility window (from its first image to the day before a new
#' unit appears at the same site).  Only the final service row of each
#' unit retains the value; earlier rows get \code{NA}.
#'
#' @param services Output of \code{summarise_services()}.
#' @param daily2   Working daily table (after gap handling and site column).
#'
#' @return \code{services} with an extra column \code{uptime_pct}.
#' @export
attach_uptime <- function(services, daily2) {
  # 1. For each unit, find start and end, and site
  unit_spans <- daily2 %>%
    filter(oper == 1) %>%
    group_by(site, cam_id) %>%
    summarise(unit_start = min(date),
              unit_end   = max(date),
              .groups = "drop")
  
  # 2. For each site, find next unit start (if any)
  unit_spans <- unit_spans %>%
    arrange(site, unit_start) %>%
    group_by(site) %>%
    mutate(
      next_unit_start = lead(unit_start),
      window_end = ifelse(is.na(next_unit_start),
                          max(daily2$date[daily2$site == first(site)]),
                          next_unit_start - 1)
    ) %>%
    ungroup()
  
  # 3. Calculate uptime for each unit's window using reframe
  uptimes <- unit_spans %>%
    rowwise() %>%
    reframe(
      cam_id = cam_id,
      uptime_pct = {
        window_days <- seq(unit_start, window_end, by = "1 day")
        mask <- daily2$cam_id == cam_id & daily2$date %in% window_days
        alive_days <- sum(daily2$oper[mask] == 1, na.rm = TRUE)
        total_days <- length(window_days)
        round(100 * alive_days / total_days, 1)
      }
    ) %>%
    ungroup()
  
  # 4. Attach new uptime to the services summary
  services %>%
    left_join(uptimes, by = "cam_id") %>%
    group_by(cam_id) %>%
    mutate(
      uptime_pct = if_else(row_number() == n(), uptime_pct, NA_real_)
    ) %>%
    ungroup()
}

#' Reduce to daily site-level operational status
#'
#' Sets \code{oper = 1} for a site-date if *any* camera unit at that site
#' was operational, else 0.
#'
#' @param daily2 Data containing `site`, `date`, `oper`.
#'
#' @return Long data frame with columns `site`, `date`, `oper`.
#' @export
site_operational_status <- function(daily2) {
  daily2 %>%
    group_by(site, date) %>%
    summarise(oper = as.integer(any(oper == 1)), .groups = "drop")
}

#' Convert site-date table to a wide operational matrix
#'
#' Produces a 0/1 matrix with one column per site and one row per day.
#'
#' @param site_operational Output of \code{site_operational_status()}.
#'
#' @return Wide tibble: first column `date`, remaining columns = site names.
#' @export
make_op_matrix_site <- function(site_operational) {
  all_dates <- seq(min(site_operational$date), max(site_operational$date), 
                   by = "1 day")
  pivot_wider(
    site_operational,
    id_cols = date,
    names_from = site,
    values_from = oper,
    values_fill = 0
  ) %>%
    complete(date = all_dates, fill = list(oper = 0)) %>%
    arrange(date)
}

#' End-to-end pipeline: from raw records to service summary & site matrix
#'
#' Calls all helper steps in sequence.  Returns a list with:
#' \itemize{
#'   \item \code{$services} – deployment-level summary table
#'   \item \code{$op_matrix} – site-level operational matrix
#' }
#'
#' @param dat Raw camera-trap data (one row per image).
#'
#' @return Named list of two tibbles.
#' @export
analyse_cameras <- function(dat) {
  
  dat1 <- parse_columns(dat)
  # Parse and coerce columns: timestamps, numeric battery/counter values
  
  dat2 <- tag_units(dat1)
  # Assign camera unit IDs within each site by detecting resets in counters
  
  daily <- daily_aggregate(dat2)
  # Aggregate to one row per camera unit per day (image count, median battery)
  
  daily_filled <- expand_daily_series(daily)
  # Expand to a full sequence of days for each camera unit, filling zero/NA for days with no images
  
  daily2 <- detect_services(daily_filled)
  # Assign service_id to each unit's time series by detecting battery jumps (new deployments)
  
  daily2 <- flag_operational(daily2)
  # Flag operational days: camera assumed alive from first to last imaged day within each service period
  
  daily2 <- add_site_column(daily2)
  # Add 'site' column by extracting prefix from cam_id
  
  daily2 <- zero_anomalous_gaps(daily2, q = 0.99)
  # Zero-out anomalously long no-image gaps at the site level

  services <- summarise_services(daily2)
  # Summarise stats for each service period (duration, image count, battery health, etc.)
  
  service_summary <- attach_uptime(services, daily2)
  # Attach uptime % for each unit (only for final service interval)
  
  site_operational <- site_operational_status(daily2)
  # For each site-date, set operational = 1 if any unit was alive, else 0
  
  op_mat_site <- make_op_matrix_site(site_operational)
  # Pivot to wide matrix: date × site, 0/1 for operational status
  
  list(services = service_summary, 
       op_matrix = op_mat_site)
}
