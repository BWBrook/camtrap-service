# camera_ops_functions.R -------------------------------------------------
# All heavy lifting lives here.  Import the needed bits explicitly.

import::here(dplyr, `%>%`, arrange, mutate, lag, group_by, ungroup,
             select, summarise, n, first, last, across, everything,
             .from = "dplyr")
import::here(tidyr, replace_na, .from = "tidyr")
import::here(lubridate, ymd_hms, as_date, days, .from = "lubridate")
import::here(purrr, map2, .from = "purrr")

# ---------- PARAMETERS (tweak here) ------------------------------------
SERVICE_BOUNCE  <- 600   # mV jump that signals fresh batteries
MAX_GAP_FILL    <- 2     # days of silence to infill inside a deployment
# -----------------------------------------------------------------------

#-- helper: tag camera units within each site ---------------------------
tag_units <- function(df) {
  df %>% 
    arrange(site, datetime) %>%
    group_by(site) %>%
    mutate(
      li_diff = li - lag(li),
      rd_diff = rd - lag(rd),
      unit_switch = case_when(
        is.na(li_diff) ~ 0L,
        li_diff <= 0 | li_diff > 1 ~ 1L,   # LI should +1 exactly
        rd_diff < 0                ~ 1L,   # RD never decreases
        TRUE                       ~ 0L
      ),
      unit_id = cumsum(unit_switch) + 1L,
      site_unit = paste0(site, "_u", unit_id)
    ) %>% 
    ungroup() %>% 
    select(-li_diff, -rd_diff, -unit_switch)
}

#-- helper: detect service events (battery swaps) -----------------------
detect_services <- function(df_unit) {
  df_unit %>% 
    arrange(date) %>% 
    mutate(
      bl_med = median(bl, na.rm = TRUE),   # daily median already?
      bl_jump = bl - lag(bl),
      service = bl_jump >= SERVICE_BOUNCE  # big voltage rebound
    ) %>% 
    replace_na(list(service = FALSE)) %>% 
    mutate(service = replace(service, 1, TRUE),          # first obs = service
           service_id = cumsum(service))
}

#-- helper: flag operational days inside one service period -------------
flag_operational <- function(tbl_unit) {
  tbl_unit %>% 
    group_by(service_id) %>% 
    mutate(
      # 1 if we observed images
      oper_raw = count > 0,
      # fill internal gaps ≤ MAX_GAP_FILL
      oper = {
        gaps   <- !oper_raw
        blocks <- cumsum(gaps != lag(gaps, default = FALSE))
        gsize  <- ave(gaps, blocks, FUN = length)
        fill   <- gaps & (gsize <= MAX_GAP_FILL) &
                 lag(oper_raw, default = FALSE) &
                 lead(oper_raw, default = FALSE)
        as.integer(oper_raw | fill)
      }
    ) %>% 
    ungroup()
}

#-- main aggregation pipeline -------------------------------------------
analyse_cameras <- function(dat) {
  dat <- dat %>% 
    mutate(datetime = ymd_hms(datetime, tz = "UTC"),
           date     = as_date(datetime),
           bl       = as.numeric(bl),
           rd       = as.numeric(rd),
           li       = as.numeric(li))

  # 1) tag units
  dat1 <- tag_units(dat)

  # 2) daily aggregation per unit
  daily <- dat1 %>% 
    group_by(site_unit, date) %>% 
    summarise(count = n(),
              bl     = median(bl, na.rm = TRUE),
              .groups = "drop")

  # 3) identify services and operational flags
  daily2 <- daily %>% 
    group_by(site_unit) %>% 
    group_modify(~ detect_services(.x)) %>% 
    group_by(site_unit) %>% 
    group_modify(~ flag_operational(.x)) %>% 
    ungroup()

  # 4) service-level summary
  svc_summary <- daily2 %>% 
    group_by(site_unit, service_id) %>% 
    summarise(start      = first(date),
              end        = last(date),
              days       = as.integer(end - start) + 1L,
              images     = sum(count),
              min_bl     = min(bl, na.rm = TRUE),
              mean_bl    = mean(bl, na.rm = TRUE),
              uptime_pct = round(sum(oper) / days * 100, 1),
              .groups = "drop")

  # 5) wide 0/1 matrix of operational days
  all_dates <- seq(min(daily2$date), max(daily2$date), by = "1 day")
  op_mat <- tidyr::pivot_wider(
    daily2,
    id_cols   = date,
    names_from = site_unit,
    values_from = oper,
    values_fill = 0
  ) %>% 
    tidyr::complete(date = all_dates, fill = list(oper = 0)) %>% 
    arrange(date)

  list(daily = daily2,
       services = svc_summary,
       op_matrix = op_mat)
}
