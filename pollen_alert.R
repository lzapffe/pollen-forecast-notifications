# pollen_alert.R
# Checks tomorrow's pollen forecast for Oslo from several sources and posts
# to Slack if any source forecasts High or Extreme for any pollen type.

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(lubridate)
  library(glue)
  library(stringr)
  library(tibble)
})

# ---- Settings ---------------------------------------------------------------
tz          <- "Europe/Oslo"
lat         <- 59.91
lon         <- 10.75
naaf_region <- "Østlandet med Oslo"  # check the exact region name in NAAF's response
alert_level <- 3L                    # common scale: 0 None, 1 Low, 2 Moderate, 3 High, 4 Extreme
levels_en   <- c("None", "Low", "Moderate", "High", "Extreme")

# Open-Meteo gives grains/m³, not categories. Lower bounds for Low/Moderate/High/Extreme,
# applied to tomorrow's hourly peak. Starting points only: tune them against NAAF over a season.
om_thresholds <- list(
  alder   = c(1, 10, 100, 1000),
  birch   = c(1, 10, 100, 1000),
  grass   = c(1, 5, 30, 150),
  mugwort = c(1, 5, 20, 100)
)

# ---- Only run at 20:xx Oslo time (handles summer/winter time) -------------------
now <- with_tz(Sys.time(), tz)
if (!identical(Sys.getenv("FORCE_RUN"), "true") && hour(now) != 20) {
  message("Not 20:xx in Oslo (", format(now), "), skipping.")
  quit(save = "no", status = 0)
}
tomorrow <- as.Date(format(now, "%Y-%m-%d")) + 1

# ---- Source 1: NAAF ----------------------------------------------------------
# ASSUMPTION: response is a list of regions, each with name + forecast[date, description,
# pollen[name, distribution (0-4), description]], as in NAAF's older member API.
# Adjust URL, auth header and field names to the documentation you get with your key.
fetch_naaf <- function(date) {
  key <- Sys.getenv("NAAF_API_KEY")
  if (key == "") stop("NAAF_API_KEY is not set")

  resp <- request(Sys.getenv("NAAF_API_URL")) |>
    req_headers(Authorization = paste("Bearer", key)) |>  # adjust to NAAF's auth scheme
    req_timeout(30) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json()

  region <- keep(resp, ~ identical(.x$name, naaf_region))
  if (!length(region)) stop("Region '", naaf_region, "' not found in NAAF response")

  fc <- keep(region[[1]]$forecast, ~ as.Date(substr(.x$date, 1, 10)) == date)
  if (!length(fc)) return(tibble())  # nothing published (e.g. off-season)
  fc <- fc[[1]]

  map_dfr(fc$pollen, ~ tibble(
    source      = "NAAF",
    pollen      = .x$name,
    level       = as.integer(.x$distribution),
    detail      = .x$description %||% NA_character_,
    region_text = fc$description %||% NA_character_
  ))
}

# ---- Source 2: Open-Meteo (CAMS European model, free, no key) ------------------
fetch_openmeteo <- function(date) {
  vars <- paste0(names(om_thresholds), "_pollen")

  resp <- request("https://air-quality-api.open-meteo.com/v1/air-quality") |>
    req_url_query(latitude = lat, longitude = lon, timezone = tz,
                  forecast_days = 3, hourly = paste(vars, collapse = ",")) |>
    req_timeout(30) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  as_tibble(resp$hourly) |>
    mutate(day = as.Date(substr(time, 1, 10))) |>
    filter(day == date) |>
    pivot_longer(all_of(vars), names_to = "pollen", values_to = "grains") |>
    group_by(pollen) |>
    summarise(peak = suppressWarnings(max(grains, na.rm = TRUE)), .groups = "drop") |>
    mutate(
      pollen = str_remove(pollen, "_pollen"),
      level  = map2_int(pollen, peak, \(p, v) if (is.finite(v)) sum(v >= om_thresholds[[p]]) else NA_integer_),
      source = "Open-Meteo (CAMS)",
      detail = if_else(is.finite(peak), paste0("peak ", round(peak), " grains/m³"), NA_character_)
    ) |>
    select(source, pollen, level, detail)
}

# ---- Source 3: Google Pollen API (optional, needs key) ------------------------
upi_to_level <- function(v) c(0L, 1L, 1L, 2L, 3L, 4L)[v + 1]  # UPI 0-5 -> common 0-4 scale

fetch_google <- function(date) {
  key <- Sys.getenv("GOOGLE_POLLEN_KEY")
  if (key == "") return(NULL)  # skip quietly if not configured

  resp <- request("https://pollen.googleapis.com/v1/forecast:lookup") |>
    req_url_query(key = key, `location.latitude` = lat, `location.longitude` = lon,
                  days = 3, languageCode = "en", plantsDescription = "false") |>
    req_timeout(30) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json()

  day <- keep(resp$dailyInfo, ~ as.Date(sprintf("%d-%02d-%02d", .x$date$year, .x$date$month, .x$date$day)) == date)
  if (!length(day)) return(tibble())

  map_dfr(day[[1]]$plantInfo %||% list(), function(p) {
    if (is.null(p$indexInfo)) return(NULL)  # plant not in season
    tibble(source = "Google Pollen", pollen = p$displayName,
           level  = upi_to_level(p$indexInfo$value),
           detail = paste0("UPI ", p$indexInfo$value, " (", p$indexInfo$category, ")"))
  })
}

# ---- Collect -------------------------------------------------------------------
sources <- list(NAAF = fetch_naaf, `Open-Meteo (CAMS)` = fetch_openmeteo, `Google Pollen` = fetch_google)

results <- imap(sources, \(f, nm) tryCatch(
  list(data = f(tomorrow), error = NULL),
  error = \(e) { message(nm, " failed: ", conditionMessage(e)); list(data = NULL, error = conditionMessage(e)) }
))

all <- bind_rows(tibble(source = character(), pollen = character(), level = integer(), detail = character()),
                 map(results, "data")) |>
  filter(!is.na(level))
errors  <- compact(map(results, "error"))
flagged <- filter(all, level >= alert_level)

# Also warn if NAAF itself failed during the season, so a broken fetch isn't silent
naaf_down_in_season <- "NAAF" %in% names(errors) && month(now) %in% 2:9

if (nrow(flagged) == 0 && !naaf_down_in_season) {
  message("No High/Extreme forecast for ", tomorrow, ". No message sent.")
  quit(save = "no", status = 0)
}

# ---- Build Slack message ----------------------------------------------------------
header <- glue(":herb: *Pollen forecast for Oslo – {format(tomorrow, '%A %d %B')}*")

summary_line <- if (nrow(flagged)) {
  hits <- unique(glue("{flagged$pollen} – {levels_en[flagged$level + 1]} ({flagged$source})"))
  paste0(":warning: *High or extreme tomorrow:* ", paste(hits, collapse = "; "))
} else ":grey_question: No High/Extreme found, but NAAF could not be checked."

source_blocks <- map_chr(unique(all$source), function(src) {
  d <- all |> filter(source == src) |> arrange(desc(level))
  lines <- paste0(
    if_else(d$level >= alert_level, ":warning: ", "• "),
    "*", d$pollen, "*: ", levels_en[d$level + 1],
    if_else(is.na(d$detail) | d$detail == "", "", paste0(" — ", d$detail))
  )
  intro <- if (src == "NAAF" && "region_text" %in% names(d) && !is.na(d$region_text[1])) {
    paste0("\n_", d$region_text[1], "_")
  } else ""
  paste0("*", src, "*", intro, "\n", paste(lines, collapse = "\n"))
})

error_lines <- if (length(errors)) paste0(":x: Could not fetch ", names(errors), ": ", unlist(errors)) else character()

msg <- paste(c(header, summary_line, "", source_blocks, error_lines), collapse = "\n")
cat(msg, "\n")

# ---- Send ----------------------------------------------------------------------------
request(Sys.getenv("SLACK_WEBHOOK_URL")) |>
  req_body_json(list(text = msg)) |>
  req_perform()
