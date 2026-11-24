# install.packages(c("shiny", "shinyjs", "readr", "dplyr", "plotly", "jsonlite", "tidyr", "future", "future.apply", "progressr", "httr", "lubridate", "ggplot2"))

library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(plotly)
library(jsonlite)
library(tidyr)
library(future)
library(future.apply)
library(progressr)
library(httr)
library(lubridate)
library(ggplot2)

plan(multisession, workers = min(8, max(2, future::availableCores() - 1)))
handlers("shiny")

options(shiny.maxRequestSize = 30 * 1024^2)  # 30 MB, in case the .zip has heavy reviews/tags

TMDB_BASE <- "https://api.themoviedb.org/3"
TMDB_IMG  <- "https://image.tmdb.org/t/p/w500"

# ── TMDB helpers ─────────────────────────────────────────────────────────────

tmdb_cache <- new.env(hash = TRUE, parent = emptyenv())

`%||%` <- function(a, b) if (is.null(a)) b else a

tmdb_get <- function(path, api_key, ...) {
  params <- c(list(...), list(api_key = api_key))
  qs <- paste(names(params),
              sapply(params, function(x) URLencode(as.character(x), reserved = TRUE)),
              sep = "=", collapse = "&")
  url <- paste0(TMDB_BASE, path, "?", qs)
  tryCatch(fromJSON(url, simplifyVector = FALSE), error = function(e) NULL)
}

# Search + details/credits for one film. Returns a list or NULL.
fetch_film_info <- function(api_key, title, year) {
  key <- paste0(tolower(title), "|", year)
  if (exists(key, envir = tmdb_cache)) return(get(key, envir = tmdb_cache))

  info <- NULL
  search <- tmdb_get("/search/movie", api_key, query = title, year = year)

  results <- search$results
  if (!is.null(results) && length(results) > 0) {
    # Prefer exact-ish year match if available, else first result
    pick <- results[[1]]
    if (!is.na(year) && year != "") {
      exact_year <- Filter(function(r) {
        ry <- tryCatch(substr(r$release_date %||% "", 1, 4), error = function(e) "")
        !is.null(ry) && ry == as.character(year)
      }, results)
      if (length(exact_year) > 0) pick <- exact_year[[1]]
    }

    id <- pick$id
    details <- tmdb_get(paste0("/movie/", id), api_key, append_to_response = "credits")

    if (!is.null(details)) {
      crew <- details$credits$crew
      cast <- details$credits$cast

      directors <- list()
      if (!is.null(crew)) {
        dir_hits <- Filter(function(c) !is.null(c$job) && c$job == "Director", crew)
        directors <- lapply(dir_hits, function(c) {
          list(name = c$name, profile_path = c$profile_path %||% NA_character_,
               popularity = c$popularity %||% NA_real_)
        })
      }

      top_cast <- list()
      if (!is.null(cast)) {
        n <- min(5, length(cast))
        top_cast <- lapply(cast[seq_len(n)], function(c) {
          list(name = c$name, profile_path = c$profile_path %||% NA_character_,
               popularity = c$popularity %||% NA_real_)
        })
      }

      genres <- character(0)
      if (!is.null(details$genres)) {
        genres <- sapply(details$genres, function(g) g$name)
      }

      countries <- character(0)
      if (!is.null(details$production_countries)) {
        countries <- sapply(details$production_countries, function(c) c$name)
      }

      runtime <- details$runtime %||% NA_integer_

      poster_path <- pick$poster_path
      poster_url <- if (!is.null(poster_path) && !is.na(poster_path)) paste0(TMDB_IMG, poster_path) else NA_character_

      info <- list(
        title       = title,
        year        = year,
        tmdb_id     = id,
        poster_url  = poster_url,
        directors   = directors,
        cast        = top_cast,
        genres      = genres,
        countries   = countries,
        runtime     = runtime
      )
    }
  }

  assign(key, info, envir = tmdb_cache)
  info
}

# ── Profile info (username, bio, avatar) ────────────────────────────────────

read_profile_info <- function(export_dir) {
  hits <- list.files(export_dir, pattern = "^profile\\.csv$",
                      full.names = TRUE, ignore.case = TRUE)
  if (length(hits) == 0) return(NULL)
  df <- tryCatch(readr::read_csv(hits[1], show_col_types = FALSE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- normalize_cols(df)
  as.list(df[1, ])
}

fetch_avatar_url <- function(username) {
  if (is.null(username) || is.na(username) || nchar(trimws(username)) == 0) return(NA_character_)
  url <- paste0("https://letterboxd.com/", trimws(username), "/")
  html <- tryCatch({
    resp <- httr::GET(url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0 Safari/537.36"),
      httr::add_headers(Accept = "text/html,application/xhtml+xml", `Accept-Language` = "en-US,en;q=0.9"),
      httr::timeout(8))
    if (httr::status_code(resp) != 200) return(NULL)
    httr::content(resp, as = "text", encoding = "UTF-8")
  }, error = function(e) NULL)
  if (is.null(html)) return(NA_character_)

  m <- regmatches(html, regexpr(
    '<meta[^>]*property=["\']og:image["\'][^>]*content=["\']([^"\']+)["\']',
    html, perl = TRUE, ignore.case = TRUE))
  if (length(m) == 0 || nchar(m) == 0) return(NA_character_)
  sub('.*content=["\']([^"\']+)["\'].*', '\\1', m, perl = TRUE, ignore.case = TRUE)
}

# Builds a lookup from Letterboxd short URI -> (name, year), combining
# diary/watched/ratings, so we can resolve profile favorites without any
# extra network request.
build_uri_lookup <- function(export_dir) {
  find_one <- function(fname) find_export_file(export_dir, fname)
  paths <- list(find_one("diary\\.csv"), find_one("watched\\.csv"), find_one("ratings\\.csv"))
  paths <- paths[!sapply(paths, is.null)]
  if (length(paths) == 0) return(NULL)

  dfs <- lapply(paths, function(p) {
    df <- tryCatch(readr::read_csv(p, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    df <- normalize_cols(df)
    if (!all(c("name", "year", "letterboxd_uri") %in% names(df))) return(NULL)
    df[, c("name", "year", "letterboxd_uri")]
  })
  dfs <- dfs[!sapply(dfs, is.null)]
  if (length(dfs) == 0) return(NULL)

  combined <- do.call(rbind, dfs)
  combined[!duplicated(combined$letterboxd_uri), ]
}

# Resolves the profile's "Favorite Films" (a comma-separated list of
# boxd.it short URLs) into (name, year, poster) using the uri lookup above.
resolve_favorites <- function(profile, uri_lookup, films_list) {
  if (is.null(profile) || is.null(profile$favorite_films) || is.na(profile$favorite_films)) return(NULL)
  if (is.null(uri_lookup)) return(NULL)
  uris <- trimws(strsplit(profile$favorite_films, ",")[[1]])
  uris <- uris[nchar(uris) > 0]
  if (length(uris) == 0) return(NULL)

  rows <- lapply(uris, function(u) {
    hit <- uri_lookup[uri_lookup$letterboxd_uri == u, ]
    if (nrow(hit) == 0) return(NULL)
    nm <- hit$name[1]; yr <- hit$year[1]
    idx <- which(sapply(films_list, function(x) x$name == nm && x$year == yr))[1]
    poster <- if (!is.na(idx) && !is.null(films_list[[idx]]$info)) films_list[[idx]]$info$poster_url %||% NA_character_ else NA_character_
    data.frame(name = nm, year = yr, poster_url = poster, stringsAsFactors = FALSE)
  })
  rows <- rows[!sapply(rows, is.null)]
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Most recently liked films (likes/films.csv), one row per film.
# Not reachable via find_export_file since that helper skips the likes/
# folder on purpose for the main diary/watched/ratings lookups.
recently_liked_df <- function(export_dir, films_list, n = 10) {
  path <- file.path(export_dir, "likes", "films.csv")
  if (!file.exists(path)) return(NULL)
  d <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  d <- normalize_cols(d)
  if (!all(c("name", "year", "date") %in% names(d))) return(NULL)

  d$liked_date <- suppressWarnings(as.Date(d$date))
  d <- d[!is.na(d$liked_date), ]
  if (nrow(d) == 0) return(NULL)
  d <- d[order(d$liked_date, decreasing = TRUE), ]
  d <- d[!duplicated(paste(d$name, d$year)), ]
  d <- head(d, n)

  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    idx <- which(sapply(films_list, function(x) x$name == r$name && x$year == r$year))[1]
    poster <- if (!is.na(idx) && !is.null(films_list[[idx]]$info)) films_list[[idx]]$info$poster_url %||% NA_character_ else NA_character_
    data.frame(name = r$name, year = r$year, poster_url = poster, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
recent_watched_df <- function(diary_raw, films_list, n = 8) {
  if (is.null(diary_raw) || !("watched_date" %in% names(diary_raw))) return(NULL)
  d <- diary_raw
  d$wd <- suppressWarnings(as.Date(d$watched_date))
  d <- d[!is.na(d$wd), ]
  if (nrow(d) == 0) return(NULL)
  d <- d[order(d$wd, decreasing = TRUE), ]
  d <- d[!duplicated(paste(d$name, d$year)), ]
  d <- head(d, n)

  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    idx <- which(sapply(films_list, function(x) x$name == r$name && x$year == r$year))[1]
    poster <- if (!is.na(idx) && !is.null(films_list[[idx]]$info)) films_list[[idx]]$info$poster_url %||% NA_character_ else NA_character_
    data.frame(name = r$name, year = r$year, poster_url = poster, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ── CSV parsing ──────────────────────────────────────────────────────────────

normalize_cols <- function(df) {
  names(df) <- tolower(gsub("[^a-zA-Z0-9]+", "_", names(df)))
  names(df) <- gsub("_+$", "", names(df))
  df
}

read_export_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  df <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(df)) return(NULL)
  df <- normalize_cols(df)
  if (!"name" %in% names(df)) return(NULL)
  df <- df[!is.na(df$name) & nchar(trimws(df$name)) > 0, ]
  if (nrow(df) == 0) return(NULL)
  if ("year" %in% names(df)) df$year <- suppressWarnings(as.integer(df$year)) else df$year <- NA_integer_
  if ("rating" %in% names(df)) df$rating <- suppressWarnings(as.numeric(df$rating))
  df
}

# Finds a named file inside the export, preferring the top-level copy over
# any match inside deleted/orphaned/likes/lists subfolders.
find_export_file <- function(export_dir, fname) {
  top_level <- list.files(export_dir, pattern = paste0("^", fname, "$"),
                           full.names = TRUE, ignore.case = TRUE)
  if (length(top_level) > 0) return(top_level[1])

  all_hits <- list.files(export_dir, pattern = paste0("^", fname, "$"),
                          recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  all_hits <- all_hits[!grepl("(^|/)(deleted|orphaned|likes|lists)/", all_hits)]
  if (length(all_hits) == 0) return(NULL)
  all_hits[1]
}

# Reads whichever of diary.csv / watched.csv / ratings.csv are present,
# and builds one row per unique (name, year) with a watch count and a rating.
build_unique_films <- function(export_dir) {
  find_one <- function(fname) find_export_file(export_dir, fname)

  diary   <- read_export_csv(find_one("diary\\.csv"))
  watched <- read_export_csv(find_one("watched\\.csv"))
  ratings <- read_export_csv(find_one("ratings\\.csv"))

  if (!is.null(diary)) {
    diary_counts <- diary |>
      group_by(name, year) |>
      summarise(n_watched = n(),
                diary_rating = if ("rating" %in% names(diary)) mean(rating, na.rm = TRUE) else NA_real_,
                .groups = "drop") |>
      mutate(logged = TRUE)

    if (!is.null(watched)) {
      # Films marked as watched but never logged in the diary count as watched once.
      extra <- watched |>
        distinct(name, year) |>
        anti_join(diary_counts, by = c("name", "year")) |>
        mutate(n_watched = 1, diary_rating = NA_real_, logged = FALSE)
      unique_films <- bind_rows(diary_counts, extra)
    } else {
      unique_films <- diary_counts
    }
  } else if (!is.null(watched)) {
    unique_films <- watched |>
      distinct(name, year) |>
      mutate(n_watched = 1, diary_rating = NA_real_, logged = FALSE)
  } else {
    stop("No movies found in your export (checked diary.csv and watched.csv).")
  }

  if (!is.null(ratings) && "rating" %in% names(ratings)) {
    unique_films <- unique_films |>
      left_join(ratings |> select(name, year, rating), by = c("name", "year")) |>
      mutate(avg_rating = ifelse(!is.na(diary_rating), diary_rating, rating)) |>
      select(-diary_rating, -rating)
  } else {
    unique_films <- unique_films |> rename(avg_rating = diary_rating)
  }

  unique_films
}

# Given an uploaded file (zip export from Letterboxd, or a bare diary.csv),
# return a directory containing the extracted export (or a temp dir with
# just the one csv, if a bare csv was uploaded).
resolve_export_dir <- function(uploaded_path, uploaded_name) {
  is_zip <- grepl("\\.zip$", uploaded_name, ignore.case = TRUE)
  extract_dir <- file.path(tempdir(), paste0("lbx_", as.integer(Sys.time())))
  dir.create(extract_dir, showWarnings = FALSE)

  if (!is_zip) {
    file.copy(uploaded_path, file.path(extract_dir, "diary.csv"))
    return(extract_dir)
  }

  utils::unzip(uploaded_path, exdir = extract_dir)
  extract_dir
}

VIOLET_GRADIENT <- colorRampPalette(c("#3d1f6e", "#7c5cbf", "#c4a8f0"))(30)

GENRE_COLORS <- c("#378ADD", "#1D9E75", "#D4537E", "#BA7517", "#7F77DD",
                   "#D85A30", "#639922", "#993556", "#185FA5", "#0F6E56", "#534AB7", "#C4760A")

# GENRE_COLORS covers 12 well-separated hand-picked hues; if more are needed
# (up to ~19), fill in with evenly-spaced generated hues so every genre still
# gets a visually distinct color.
get_genre_colors <- function(n) {
  if (n <= length(GENRE_COLORS)) return(GENRE_COLORS[seq_len(n)])
  extra_n <- n - length(GENRE_COLORS)
  extra <- sapply(seq(0, 1, length.out = extra_n + 1)[seq_len(extra_n)], function(h) {
    grDevices::hsv(h, 0.55, 0.85)
  })
  c(GENRE_COLORS, extra)
}

bar_colors <- function(n) {
  idx <- round(seq(length(VIOLET_GRADIENT), 1, length.out = n))
  VIOLET_GRADIENT[idx]
}

# Disables the click-drag rectangle zoom (confusing/accidental on mobile)
# and hides the modebar toolbar, applied consistently to every chart.
style_plotly <- function(p) {
  p |>
    layout(dragmode = FALSE) |>
    plotly::config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
}

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
# Opcional: URL del formulario de feedback (Formspree). Si no esta definida, el feedback se oculta.
FORMSPREE_ENDPOINT <- Sys.getenv("FORMSPREE_ENDPOINT")

first_non_na <- function(x) {
  nn <- x[!is.na(x)]
  if (length(nn) > 0) nn[1] else NA_character_
}

# Aggregates a person/genre field ("directors", "cast", or "genres") across
# a films subset (data.frame with name/year/n_watched), counting unique movies.
aggregate_field <- function(films_subset_df, base_films_list, field) {
  if (is.null(films_subset_df) || nrow(films_subset_df) == 0) return(data.frame())
  rows <- lapply(seq_len(nrow(films_subset_df)), function(i) {
    r <- films_subset_df[i, ]
    idx <- which(sapply(base_films_list, function(x) x$name == r$name && x$year == r$year))[1]
    if (is.na(idx) || is.null(base_films_list[[idx]]$info)) return(NULL)
    items <- base_films_list[[idx]]$info[[field]]
    if (length(items) == 0) return(NULL)
    if (field == "genres") {
      data.frame(key = items, popularity = NA_real_, stringsAsFactors = FALSE)
    } else {
      do.call(rbind, lapply(items, function(it) {
        data.frame(key = it$name, popularity = it$popularity %||% NA_real_, stringsAsFactors = FALSE)
      }))
    }
  })
  rows <- rows[!sapply(rows, is.null)]
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows) |>
    group_by(key) |>
    summarise(count = n(), popularity = first_non_na(popularity), .groups = "drop")
}

pick_top_row <- function(df, break_tie_least_popular = FALSE) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  mx <- max(df$count)
  ties <- df[df$count == mx, ]
  if (nrow(ties) == 1 || !break_tie_least_popular) return(ties[1, ])
  ties <- ties[order(ties$popularity, na.last = TRUE), ]
  ties[1, ]
}

# Up to 4 posters per value of `field` ("countries" or "genres"), for a
# hover collage: most-recently logged (diary) movies first, filled with a
# random watched-only movie sharing that value if there aren't 4 logged ones.
build_field_posters <- function(films_list, diary_raw, field) {
  if (is.null(films_list)) return(list())

  diary_dates <- NULL
  if (!is.null(diary_raw) && "watched_date" %in% names(diary_raw)) {
    dd <- diary_raw
    dd$wd <- suppressWarnings(as.Date(dd$watched_date))
    dd <- dd[!is.na(dd$wd), ]
    dd <- dd[order(dd$wd, decreasing = TRUE), ]
    diary_dates <- dd[!duplicated(paste(dd$name, dd$year)), c("name", "year", "wd")]
  }

  rows <- lapply(films_list, function(x) {
    vals <- if (!is.null(x$info)) x$info[[field]] else character(0)
    if (length(vals) == 0) return(NULL)
    poster <- if (!is.null(x$info)) x$info$poster_url %||% NA_character_ else NA_character_
    if (is.na(poster)) return(NULL)
    do.call(rbind, lapply(vals, function(v) {
      data.frame(key = v, name = x$name, year = x$year,
                 poster_url = poster, logged = isTRUE(x$logged), stringsAsFactors = FALSE)
    }))
  })
  rows <- rows[!sapply(rows, is.null)]
  if (length(rows) == 0) return(list())
  df <- do.call(rbind, rows)

  if (!is.null(diary_dates)) {
    df <- merge(df, diary_dates, by = c("name", "year"), all.x = TRUE)
  } else {
    df$wd <- as.Date(NA)
  }

  by_key <- split(df, df$key)
  lapply(by_key, function(kd) {
    logged_kd <- kd[kd$logged & !is.na(kd$wd), ]
    logged_kd <- logged_kd[order(logged_kd$wd, decreasing = TRUE), ]
    picks <- unique(logged_kd$poster_url)
    if (length(picks) < 4) {
      remaining <- setdiff(unique(kd$poster_url), picks)
      if (length(remaining) > 0) {
        fill_n <- min(4 - length(picks), length(remaining))
        picks <- c(picks, sample(remaining, fill_n))
      }
    }
    head(picks, 4)
  })
}

# Builds the same stats used in the main Summary tab, for any films subset
# (data.frame with name/year/n_watched/avg_rating). Used for "This Year" tabs.
build_mini_summary <- function(films_subset_df, base_films_list) {
  df <- films_subset_df
  if (is.null(df) || nrow(df) == 0) return(NULL)

  n_unique  <- nrow(df)
  n_watches <- sum(df$n_watched)
  n_rewatch <- sum(pmax(df$n_watched - 1, 0))

  total_minutes <- sum(sapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    idx <- which(sapply(base_films_list, function(x) x$name == r$name && x$year == r$year))[1]
    rt <- if (!is.na(idx) && !is.null(base_films_list[[idx]]$info)) base_films_list[[idx]]$info$runtime %||% NA_real_ else NA_real_
    if (is.na(rt)) 0 else rt * r$n_watched
  }))
  total_hours <- round(total_minutes / 60)

  rated <- if ("avg_rating" %in% names(df)) df$avg_rating[!is.na(df$avg_rating)] else numeric(0)
  avg_rating <- if (length(rated) > 0) round(mean(rated), 2) else NA_real_
  n_rated <- length(rated)

  top_director <- pick_top_row(aggregate_field(df, base_films_list, "directors"), break_tie_least_popular = TRUE)
  top_actor    <- pick_top_row(aggregate_field(df, base_films_list, "cast"), break_tie_least_popular = TRUE)
  top_genre    <- pick_top_row(aggregate_field(df, base_films_list, "genres"), break_tie_least_popular = FALSE)

  years <- df$year[!is.na(df$year)]
  oldest_year <- if (length(years) > 0) min(years) else NA
  newest_year <- if (length(years) > 0) max(years) else NA

  list(
    n_unique = n_unique, n_watches = n_watches, n_rewatch = n_rewatch,
    total_hours = total_hours, avg_rating = avg_rating, n_rated = n_rated,
    top_director = top_director, top_actor = top_actor, top_genre = top_genre,
    oldest_year = oldest_year, newest_year = newest_year
  )
}

card <- function(value, label, sub = NULL) {
  div(class = "stat-card",
    div(class = "stat-value", value),
    div(class = "stat-label", label),
    if (!is.null(sub)) div(class = "stat-sub", sub) else NULL
  )
}

render_mini_stat_cards <- function(s) {
  div(class = "stat-grid",
    card(s$n_unique, "Unique movies"),
    card(s$n_watches, "Total watches", paste0(s$n_rewatch, " rewatches")),
    card(paste0(s$total_hours, "h"), "Time watched", paste0("(~", round(s$total_hours / 24, 1), " days)")),
    card(if (!is.na(s$avg_rating)) s$avg_rating else "—", "Average rating",
         paste0("across ", s$n_rated, " rated movies")),
    card(if (!is.null(s$top_director)) s$top_director$key else "—", "Top director",
         if (!is.null(s$top_director)) paste0(s$top_director$count, " movies") else NULL),
    card(if (!is.null(s$top_actor)) s$top_actor$key else "—", "Top actor",
         if (!is.null(s$top_actor)) paste0(s$top_actor$count, " movies") else NULL),
    card(if (!is.null(s$top_genre)) s$top_genre$key else "—", "Top genre",
         if (!is.null(s$top_genre)) paste0(s$top_genre$count, " movies") else NULL),
    card(paste0(s$oldest_year, " – ", s$newest_year), "Release year range")
  )
}

# A horizontal row of poster thumbnails with a caption underneath each.
render_poster_row <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  div(class = "poster-row",
    lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      div(class = "poster-row-item",
        if (!is.null(r$poster_url) && !is.na(r$poster_url)) tags$img(src = r$poster_url) else div(class = "poster-placeholder"),
        div(class = "poster-row-caption", paste0(r$name, " (", r$year, ")"))
      )
    })
  )
}

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;500;700&display=swap"),
    tags$style(HTML("
      body { background:#141414; color:#e8e6e0; font-family:'Space Grotesk', sans-serif; }
      .well { background:#1e1e1e; border:none; }
      .form-control { background:#2a2a2a; color:#e8e6e0; border:1px solid #3a3a3a; }
      .btn-primary { background:#7c5cbf; border:none; }
      .error-box { background:#3a1c1c; color:#f0a; padding:10px; border-radius:6px; margin-top:10px; }
      .loading-bar { color:#c4a8f0; padding:10px; }
      #poster-panel {
        position:fixed; display:none; z-index:9999; background:#1a1a1a;
        border:1px solid #7c5cbf; border-radius:8px; padding:10px; width:180px;
        box-shadow:0 4px 20px rgba(0,0,0,0.6); pointer-events:none;
      }
      #poster-panel img { width:100%; border-radius:4px; }
      #poster-panel .p-title { font-weight:700; margin-top:6px; font-size:13px; }
      #poster-panel .p-sub { color:#999993; font-size:11px; }
      #poster-panel .poster-collage { display:grid; grid-template-columns:1fr 1fr; gap:3px; }
      #poster-panel .poster-collage img { aspect-ratio:2/3; object-fit:cover; }
      .stat-grid { display:flex; flex-wrap:wrap; gap:16px; margin-top:10px; }
      .stat-card {
        background:#1e1e1e; border:1px solid #2a2a2a; border-radius:10px;
        padding:18px 20px; min-width:200px; flex:1 1 200px;
      }
      .stat-card .stat-value { font-size:28px; font-weight:700; color:#c4a8f0; line-height:1.2; }
      .stat-card .stat-label { font-size:12px; color:#999993; margin-top:4px; text-transform:uppercase; letter-spacing:0.03em; }
      .stat-card .stat-sub { font-size:12px; color:#666660; margin-top:2px; }
      .profile-header { display:flex; align-items:center; gap:16px; margin-bottom:20px; }
      .profile-header img { width:64px; height:64px; border-radius:50%; object-fit:cover; border:2px solid #7c5cbf; }
      .profile-header .profile-name { font-size:20px; font-weight:700; color:#e8e6e0; }
      .profile-header .profile-name a { color:#c4a8f0; text-decoration:none; }
      .profile-header .profile-name a:hover { text-decoration:underline; }
      .profile-header .profile-bio { font-size:13px; color:#999993; margin-top:2px; max-width:500px; }
      .profile-header .profile-joined { font-size:11px; color:#666660; margin-top:2px; }
      #poster-panel .p-close {
        position:absolute; top:4px; right:6px; color:#999993; font-size:14px;
        cursor:pointer; display:none; pointer-events:auto;
      }
      @media (max-width: 768px) {
        #poster-panel {
          left:50% !important; right:auto !important; bottom:16px !important; top:auto !important;
          transform:translateX(-50%); pointer-events:auto; width:220px;
        }
        #poster-panel .p-close { display:block; }
      }
      .mini-section-title { font-size:13px; color:#999993; text-transform:uppercase; letter-spacing:0.03em; margin:18px 0 6px; }
      .poster-row { display:flex; gap:10px; overflow-x:auto; padding-bottom:6px; }
      .poster-row-item { width:90px; flex:0 0 auto; }
      .poster-row-item img { width:100%; border-radius:4px; display:block; }
      .poster-placeholder { width:90px; height:135px; background:#2a2a2a; border-radius:4px; }
      .poster-row-caption { font-size:10px; color:#999993; margin-top:4px; text-align:center; line-height:1.3; }
      .app-footer { margin-top:24px; padding-top:14px; border-top:1px solid #2a2a2a; font-size:12px; color:#666660; }
      .app-footer a { color:#c4a8f0; text-decoration:none; }
      .app-footer a:hover { text-decoration:underline; }
      .feedback-box textarea { min-height:70px; }
      .feedback-status { font-size:12px; margin-top:6px; }
      .feedback-status.ok { color:#8fce8f; }
      .feedback-status.err { color:#f08a8a; }
    ")),
    tags$script(HTML("
      document.addEventListener('mousemove', function(e) {
        var el = document.getElementById('poster-panel');
        if (el && el.style.display === 'block') {
          var w = el.offsetWidth || 180;
          el.style.left = (e.clientX - w - 20) + 'px';
          el.style.top  = (e.clientY - 40) + 'px';
        }
      });

      function hidePosterPanel() {
        var el = document.getElementById('poster-panel');
        if (el) el.style.display = 'none';
      }

      ['chart_movies', 'chart_directors', 'chart_actors', 'chart_countries', 'chart_genres'].forEach(function(id) {
        document.addEventListener('DOMContentLoaded', function() {
          var attach = function() {
            var gd = document.getElementById(id);
            if (gd) { gd.addEventListener('mouseleave', hidePosterPanel); }
            else { setTimeout(attach, 500); }
          };
          attach();
        });
      });
    "))
  ),

  div(id = "poster-panel"),

  titlePanel("Your Letterboxd, analyzed"),

  sidebarLayout(
    sidebarPanel(
      fileInput("diary_csv", "Upload the .zip Letterboxd gives you",
                accept = c(".zip", ".csv")),
      actionButton("analyze", "Analyze my history", class = "btn-primary"),
      actionButton("demo", "Try with test data", class = "btn-default"),
      tags$small(class = "help-block", "No Letterboxd data handy? Explore the app with a fictional account."),
      tags$hr(),
      checkboxInput("count_total_watches", "Count total watches for Directors/Actors/Genres (instead of unique movies)",
                    value = FALSE),
      tags$hr(),
      checkboxInput("use_year_filter", "Filter by year logged", value = FALSE),
      conditionalPanel(
        condition = "input.use_year_filter == true",
        numericInput("start_year", "From year logged", value = 1900, min = 1900, max = 2100, step = 1),
        numericInput("end_year", "To year logged", value = as.integer(format(Sys.Date(), "%Y")),
                     min = 1900, max = 2100, step = 1)
      ),
      tags$small("Only applies to movies logged with a real watched date in your diary (watched-only entries without a diary date are excluded when this filter is active)."),
      tags$hr(),
      tags$small(
        "How do I get the .zip? On Letterboxd's website (letterboxd.com — this isn't available in the mobile app): Settings → Data → Export Your Data. Upload it as-is, no need to unzip.",
        tags$br(), tags$br(),
        "Direct link: ", tags$a(href = "https://letterboxd.com/settings/data/", target = "_blank", "letterboxd.com/settings/data/"),
        tags$br(), tags$br(),
        "On mobile: if the download seems to hang or misbehave in your regular browser, try opening that link in an incognito/private tab and logging in there — it tends to work more reliably.",
        tags$br(), tags$br(),
        tags$b("Heads up: "), "analyzing your history takes a few minutes (it depends on how many movies you've watched), and the app runs noticeably better on desktop than on mobile.",
        tags$br(), tags$br(),
        tags$em("This product uses the TMDB API but is not endorsed or certified by TMDB.")
      ),
      tags$hr(),
      uiOutput("feedback_ui"),
      div(class = "app-footer",
        "Made by ",
        tags$a(href = "https://victorurrafuentes.github.io/portafolio-riesgo", target = "_blank", "V\u00edctor Urra Fuentes")
      )
    ),

    mainPanel(
      uiOutput("status"),
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Summary", uiOutput("summary_panel")),
        tabPanel("This Year (Logged)", uiOutput("summary_this_year")),
        tabPanel("This Year (Releases)",
                 numericInput("release_year_filter", "Year", value = CURRENT_YEAR,
                              min = 1900, max = 2100, step = 1),
                 uiOutput("summary_this_year_released")),
        tabPanel("Yearly Activity", plotlyOutput("chart_yearly_activity", height = "500px")),
        tabPanel("Top Directors", plotlyOutput("chart_directors", height = "600px")),
        tabPanel("Top Actors", plotlyOutput("chart_actors", height = "600px")),
        tabPanel("Top Genres", plotlyOutput("chart_genres", height = "600px")),
        tabPanel("Top Countries", plotlyOutput("chart_countries", height = "600px")),
        tabPanel("Genres by Decade", plotlyOutput("chart_genre_decades", height = "600px")),
        tabPanel("Diary Habits", plotlyOutput("chart_diary_habits", height = "500px")),
        tabPanel("Top Movies", plotlyOutput("chart_movies", height = "600px"))
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  rv <- reactiveValues(
    diary   = NULL,
    diary_raw = NULL,
    uri_lookup = NULL,
    export_dir = NULL,
    films   = NULL,   # per unique film: title, year, n_watched, avg_rating, tmdb info
    profile = NULL,
    error   = NULL,
    loading = FALSE,
    is_demo = FALSE
  )

  output$status <- renderUI({
    if (rv$loading) {
      return(div(class = "loading-bar",
        tags$span("⏳ "),
        "Matching your movies against TMDB, this can take a while depending on how big your history is..."))
    }
    if (!is.null(rv$error)) return(div(class = "error-box", paste("Error:", rv$error)))
    NULL
  })

  output$feedback_ui <- renderUI({
    if (nchar(FORMSPREE_ENDPOINT) == 0) return(NULL)
    if (length(rv$films) == 0 || isTRUE(rv$is_demo)) {
      return(div(class = "feedback-box",
        tags$small(tags$b("Feedback")),
        tags$br(),
        tags$small(em("Analyze your own data first to unlock feedback."))
      ))
    }
    div(class = "feedback-box",
      tags$small(tags$b("Feedback")),
      textAreaInput("feedback_text", NULL, placeholder = "Bugs, ideas, anything...", width = "100%"),
      actionButton("send_feedback", "Send feedback", class = "btn-primary"),
      uiOutput("feedback_status")
    )
  })

  feedback_status <- reactiveVal(NULL)

  observeEvent(input$send_feedback, {
    req(length(rv$films) > 0)  # guard: only after a successful analysis

    msg <- trimws(input$feedback_text %||% "")
    if (nchar(msg) == 0) {
      feedback_status(list(ok = FALSE, text = "Write something first!"))
      return()
    }
    username <- if (!is.null(rv$profile) && !is.null(rv$profile$username)) rv$profile$username else "unknown"

    result <- tryCatch({
      resp <- httr::POST(
        FORMSPREE_ENDPOINT,
        body = list(message = msg, username = username, `_subject` = paste0("boxd-stats feedback from ", username)),
        encode = "form",
        httr::timeout(8)
      )
      httr::status_code(resp) %in% c(200, 201)
    }, error = function(e) FALSE)

    if (isTRUE(result)) {
      feedback_status(list(ok = TRUE, text = "Thanks, sent!"))
      updateTextAreaInput(session, "feedback_text", value = "")
    } else {
      feedback_status(list(ok = FALSE, text = "Couldn't send it, try again later."))
    }
  })

  output$feedback_status <- renderUI({
    s <- feedback_status()
    req(!is.null(s))
    div(class = paste("feedback-status", if (s$ok) "ok" else "err"), s$text)
  })

  # Hide the poster/collage panel whenever the active tab changes — it was
  # getting stuck visible after switching to a tab with no hover mechanism.
  observeEvent(input$main_tabs, {
    shinyjs::runjs("document.getElementById('poster-panel').style.display='none';")
  }, ignoreInit = TRUE)

  observeEvent(input$analyze, {
    req(input$diary_csv)
    run_analysis(input$diary_csv$datapath, input$diary_csv$name, is_demo = FALSE)
  })

  observeEvent(input$demo, {
    demo_path <- "demo_export.zip"
    if (!file.exists(demo_path)) {
      rv$error <- "The test data file is missing from the server."
      return()
    }
    run_analysis(demo_path, "demo_export.zip", is_demo = TRUE)
  })

  run_analysis <- function(upload_path, upload_name, is_demo = FALSE) {
    rv$error   <- NULL
    rv$films   <- NULL
    rv$is_demo <- is_demo

    api_key <- Sys.getenv("TMDB_API_KEY")
    if (nchar(api_key) == 0) {
      rv$error <- "The TMDB_API_KEY environment variable isn't set on the server."
      return()
    }

    rv$loading <- TRUE

    tryCatch({
      export_dir   <- resolve_export_dir(upload_path, upload_name)
      unique_films <- build_unique_films(export_dir)

      profile <- read_profile_info(export_dir)
      if (!is.null(profile)) {
        profile$avatar_url <- if (is_demo) NA_character_ else tryCatch(fetch_avatar_url(profile$username), error = function(e) NA_character_)
      }
      rv$profile <- profile

      diary_path <- find_export_file(export_dir, "diary\\.csv")
      rv$diary_raw <- if (!is.null(diary_path)) read_export_csv(diary_path) else NULL

      rv$uri_lookup <- build_uri_lookup(export_dir)
      rv$export_dir <- export_dir

      n_total <- nrow(unique_films)
      if (n_total == 0) stop("No movies found in your export. Are you sure this is your Letterboxd data export?")

      withProgress(message = paste0("Looking up ", n_total, " movies on TMDB (running in parallel)..."), value = 0, {
        p <- progressor(along = seq_len(n_total))
        results <- future_lapply(seq_len(n_total), function(i) {
          f <- unique_films[i, ]
          info <- tryCatch(fetch_film_info(api_key, f$name, f$year), error = function(e) NULL)
          p(message = f$name)
          list(
            name       = f$name,
            year       = f$year,
            n_watched  = f$n_watched,
            avg_rating = f$avg_rating,
            logged     = f$logged,
            info       = info
          )
        }, future.seed = TRUE)
      })

      rv$films <- results

      watched_years <- NULL
      if (!is.null(rv$diary_raw) && "watched_date" %in% names(rv$diary_raw)) {
        wd <- suppressWarnings(as.Date(rv$diary_raw$watched_date))
        watched_years <- suppressWarnings(as.integer(format(wd, "%Y")))
        watched_years <- watched_years[!is.na(watched_years)]
      }
      if (length(watched_years) > 0) {
        updateNumericInput(session, "start_year", value = min(watched_years))
        updateNumericInput(session, "end_year", value = max(watched_years))
      }
    }, error = function(e) {
      rv$error <- conditionMessage(e)
    })

    rv$loading <- FALSE
  }

  # Build a tidy data frame from rv$films for plotting
  films_df <- reactive({
    req(length(rv$films) > 0)
    do.call(rbind, lapply(rv$films, function(x) {
      poster <- if (!is.null(x$info)) x$info$poster_url %||% NA_character_ else NA_character_
      data.frame(
        name       = x$name,
        year       = x$year,
        n_watched  = x$n_watched,
        avg_rating = round(x$avg_rating, 2),
        logged     = isTRUE(x$logged),
        poster_url = poster,
        stringsAsFactors = FALSE
      )
    }))
  })

  # Diary rows whose Watched Date falls within the selected year range.
  # Only diary-logged entries have a real watched date, so watched-only
  # entries (no diary log) are excluded whenever this filter is applied.
  filtered_diary <- reactive({
    req(!is.null(rv$diary_raw), "watched_date" %in% names(rv$diary_raw))
    d <- rv$diary_raw
    wd <- suppressWarnings(as.Date(d$watched_date))
    d$watched_year <- suppressWarnings(as.integer(format(wd, "%Y")))
    d <- d[!is.na(d$watched_year), ]

    sy <- input$start_year
    ey <- input$end_year
    if (!is.null(sy) && !is.null(ey) && !is.na(sy) && !is.na(ey)) {
      d <- d[d$watched_year >= sy & d$watched_year <= ey, ]
    }
    d
  })

  # One row per unique (name, year) film. If the year-logged filter is off,
  # this is the full history (diary + watched-only). If it's on, watch counts
  # are recomputed from only the diary entries within the selected range
  # (watched-only entries have no real watched date, so they drop out).
  filtered_films_df <- reactive({
    if (!isTRUE(input$use_year_filter)) return(films_df())

    d <- filtered_diary()
    req(nrow(d) > 0)

    agg <- d |>
      group_by(name, year) |>
      summarise(n_watched = n(),
                avg_rating = if ("rating" %in% names(d)) mean(rating, na.rm = TRUE) else NA_real_,
                .groups = "drop")

    rows <- lapply(seq_len(nrow(agg)), function(i) {
      r <- agg[i, ]
      idx <- which(sapply(rv$films, function(x) x$name == r$name && x$year == r$year))[1]
      poster <- if (!is.na(idx) && !is.null(rv$films[[idx]]$info)) rv$films[[idx]]$info$poster_url %||% NA_character_ else NA_character_
      data.frame(name = r$name, year = r$year, n_watched = r$n_watched,
                 avg_rating = round(r$avg_rating, 2), logged = TRUE,
                 poster_url = poster, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  })

  # Same year filter, but returning a rv$films-like list with n_watched
  # recomputed for the selected range (needed for runtime/hours sums).
  current_films_list <- reactive({
    if (!isTRUE(input$use_year_filter)) {
      req(length(rv$films) > 0)
      return(rv$films)
    }

    df <- filtered_films_df()
    req(!is.null(df), nrow(df) > 0)
    lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      idx <- which(sapply(rv$films, function(x) x$name == r$name && x$year == r$year))[1]
      info <- if (!is.na(idx)) rv$films[[idx]]$info else NULL
      list(name = r$name, year = r$year, n_watched = r$n_watched,
           avg_rating = r$avg_rating, logged = TRUE, info = info)
    })
  })

  # ── Summary ──
  # Picks the row(s) with the max count; if there's a tie, keeps the
  # least-popular one according to TMDB's popularity score.
  pick_top_least_popular <- function(df) {
    if (nrow(df) == 0) return(NULL)
    max_count <- max(df$count)
    ties <- df[df$count == max_count, ]
    if (nrow(ties) == 1) return(ties)
    ties <- ties[order(ties$popularity, na.last = TRUE), ]
    ties[1, ]
  }

  summary_stats <- reactive({
    df <- filtered_films_df()
    req(!is.null(df), nrow(df) > 0)

    n_unique   <- nrow(df)
    n_watches  <- sum(df$n_watched)
    n_rewatch  <- sum(pmax(df$n_watched - 1, 0))
    n_logged   <- sum(df$logged)
    n_unlogged <- sum(!df$logged)

    total_minutes <- sum(sapply(current_films_list(), function(x) {
      rt <- if (!is.null(x$info)) x$info$runtime %||% NA_real_ else NA_real_
      if (is.na(rt)) 0 else rt * x$n_watched
    }))
    total_hours <- round(total_minutes / 60)

    total_minutes_diary <- sum(sapply(current_films_list(), function(x) {
      if (!isTRUE(x$logged)) return(0)
      rt <- if (!is.null(x$info)) x$info$runtime %||% NA_real_ else NA_real_
      if (is.na(rt)) 0 else rt * x$n_watched
    }))
    total_hours_diary <- round(total_minutes_diary / 60)

    rated <- df$avg_rating[!is.na(df$avg_rating)]
    avg_rating <- if (length(rated) > 0) round(mean(rated), 2) else NA_real_
    n_rated <- length(rated)

    top_director_row <- pick_top_least_popular(directors_df())
    top_actor_row    <- pick_top_least_popular(actors_df())
    top_genre        <- genres_df()
    top_genre_row    <- if (nrow(top_genre) > 0) top_genre[which.max(top_genre$count), ] else NULL

    years <- df$year[!is.na(df$year)]
    oldest_year <- if (length(years) > 0) min(years) else NA
    newest_year <- if (length(years) > 0) max(years) else NA

    decade_counts <- NULL
    if (length(years) > 0) {
      decades <- (years %/% 10) * 10
      dt <- sort(table(decades), decreasing = TRUE)
      decade_counts <- names(dt)[1]
    }

    list(
      n_unique      = n_unique,
      n_watches     = n_watches,
      n_rewatch     = n_rewatch,
      n_logged      = n_logged,
      n_unlogged    = n_unlogged,
      total_hours   = total_hours,
      total_hours_diary = total_hours_diary,
      avg_rating    = avg_rating,
      n_rated       = n_rated,
      top_director  = top_director_row,
      top_actor     = top_actor_row,
      top_genre     = top_genre_row,
      oldest_year   = oldest_year,
      newest_year   = newest_year,
      top_decade    = decade_counts
    )
  })

  output$summary_panel <- renderUI({
    s <- summary_stats()
    prof <- rv$profile

    avatar_src <- if (!is.null(prof) && !is.null(prof$avatar_url) && !is.na(prof$avatar_url)) prof$avatar_url else NULL

    profile_header <- if (!is.null(prof) && !is.null(prof$username)) {
      profile_url <- paste0("https://letterboxd.com/", prof$username, "/")
      div(class = "profile-header",
        if (!is.null(avatar_src)) tags$img(src = avatar_src) else NULL,
        div(
          div(class = "profile-name", tags$a(href = profile_url, target = "_blank", prof$username)),
          if (!is.null(prof$bio) && !is.na(prof$bio) && nchar(prof$bio) > 0)
            div(class = "profile-bio", prof$bio) else NULL,
          if (!is.null(prof$date_joined) && !is.na(prof$date_joined))
            div(class = "profile-joined", paste0("On Letterboxd since ", prof$date_joined)) else NULL
        )
      )
    } else NULL

    favorites_df <- resolve_favorites(prof, rv$uri_lookup, rv$films)
    recent_df    <- recent_watched_df(rv$diary_raw, rv$films, n = 10)
    liked_df     <- if (!is.null(rv$export_dir)) recently_liked_df(rv$export_dir, rv$films, n = 10) else NULL

    tagList(
      profile_header,
      if (!is.null(favorites_df)) tagList(
        div(class = "mini-section-title", "Favorites"),
        render_poster_row(favorites_df)
      ) else NULL,
      if (!is.null(recent_df)) tagList(
        div(class = "mini-section-title", "Recently watched"),
        render_poster_row(recent_df)
      ) else NULL,
      if (!is.null(liked_df)) tagList(
        div(class = "mini-section-title", "Recently liked"),
        render_poster_row(liked_df)
      ) else NULL,
      div(class = "stat-grid",
      card(s$n_unique, "Unique movies watched"),
      card(s$n_watches, "Total watches", paste0(s$n_rewatch, " rewatches")),
      card(paste0(s$n_logged, " / ", s$n_unlogged), "Logged in diary / watched only",
           paste0(round(100 * s$n_logged / s$n_unique), "% logged with a date")),
      card(paste0(s$total_hours, "h"), "Total time watched",
           paste0("(~", round(s$total_hours / 24, 1), " days)")),
      card(paste0(s$total_hours_diary, "h"), "Time watched (diary only)",
           paste0("(~", round(s$total_hours_diary / 24, 1), " days)")),
      card(if (!is.na(s$avg_rating)) s$avg_rating else "—", "Average rating",
           paste0("across ", s$n_rated, " rated movies")),
      card(if (!is.null(s$top_director)) s$top_director$director else "—", "Most-watched director",
           if (!is.null(s$top_director)) paste0(s$top_director$count, " movies") else NULL),
      card(if (!is.null(s$top_actor)) s$top_actor$actor else "—", "Most-watched actor",
           if (!is.null(s$top_actor)) paste0(s$top_actor$count, " movies") else NULL),
      card(if (!is.null(s$top_genre)) s$top_genre$genre else "—", "Favorite genre",
           if (!is.null(s$top_genre)) paste0(s$top_genre$count, " movies") else NULL),
      card(paste0(s$oldest_year, " – ", s$newest_year), "Release year range"),
      card(paste0(s$top_decade, "s"), "Most-watched decade")
      )
    )
  })

  # ── This Year (Logged) ──
  this_year_logged_df <- reactive({
    req(!is.null(rv$diary_raw), "watched_date" %in% names(rv$diary_raw))
    d <- rv$diary_raw
    wd <- suppressWarnings(as.Date(d$watched_date))
    yr <- suppressWarnings(as.integer(format(wd, "%Y")))
    d <- d[!is.na(yr) & yr == CURRENT_YEAR, ]
    if (nrow(d) == 0) return(data.frame())
    d |> group_by(name, year) |>
      summarise(n_watched = n(),
                avg_rating = if ("rating" %in% names(d)) mean(rating, na.rm = TRUE) else NA_real_,
                .groups = "drop")
  })

  output$summary_this_year <- renderUI({
    req(!is.null(rv$films))
    df <- this_year_logged_df()
    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "error-box", paste0("No movies logged in ", CURRENT_YEAR, " yet.")))
    }
    s <- build_mini_summary(df, rv$films)
    tagList(
      h4(paste0(CURRENT_YEAR, " in review — movies you logged this year")),
      render_mini_stat_cards(s)
    )
  })

  # ── This Year (Releases) ──
  this_year_released_df <- reactive({
    req(!is.null(rv$films), !is.null(input$release_year_filter))
    yr <- input$release_year_filter
    rows <- lapply(rv$films, function(x) {
      if (is.na(x$year) || x$year != yr) return(NULL)
      poster <- if (!is.null(x$info)) x$info$poster_url %||% NA_character_ else NA_character_
      data.frame(name = x$name, year = x$year, n_watched = x$n_watched,
                 avg_rating = x$avg_rating, poster_url = poster, stringsAsFactors = FALSE)
    })
    rows <- rows[!sapply(rows, is.null)]
    if (length(rows) == 0) return(data.frame())
    do.call(rbind, rows)
  })

  output$summary_this_year_released <- renderUI({
    req(!is.null(rv$films), !is.null(input$release_year_filter))
    yr <- input$release_year_filter
    df <- this_year_released_df()
    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "error-box", paste0("You haven't watched any ", yr, " releases yet.")))
    }
    s <- build_mini_summary(df, rv$films)
    top10 <- df |> arrange(desc(n_watched)) |> slice_head(n = 10)
    tagList(
      h4(paste0(yr, " releases you've watched")),
      render_mini_stat_cards(s),
      div(class = "mini-section-title", paste0(yr, " releases (up to 10)")),
      render_poster_row(top10)
    )
  })

  # ── Yearly Activity: movies logged per year + hours watched per year ──
  yearly_activity_df <- reactive({
    req(!is.null(rv$diary_raw), "watched_date" %in% names(rv$diary_raw), !is.null(rv$films))
    d <- rv$diary_raw
    wd <- suppressWarnings(as.Date(d$watched_date))
    yr <- suppressWarnings(as.integer(format(wd, "%Y")))
    d <- d[!is.na(yr), ]
    d$watched_year <- yr[!is.na(yr)]
    req(nrow(d) > 0)

    d$runtime <- sapply(seq_len(nrow(d)), function(i) {
      idx <- which(sapply(rv$films, function(x) x$name == d$name[i] && x$year == d$year[i]))[1]
      if (is.na(idx) || is.null(rv$films[[idx]]$info)) return(NA_real_)
      rv$films[[idx]]$info$runtime %||% NA_real_
    })

    d |> group_by(watched_year) |>
      summarise(n_logged = n(), total_hours = round(sum(runtime, na.rm = TRUE) / 60), .groups = "drop") |>
      arrange(watched_year)
  })

  output$chart_yearly_activity <- renderPlotly({
    df <- yearly_activity_df()
    req(nrow(df) > 0)

    plot_ly(df, x = ~watched_year) |>
      add_bars(y = ~n_logged, name = "movies logged",
               marker = list(color = "#7c5cbf"),
               hovertemplate = "<b>%{x}</b><br>movies logged: %{y}<extra></extra>") |>
      add_lines(y = ~total_hours, name = "hours watched", yaxis = "y2",
                line = list(color = "#e8b34a", width = 3),
                marker = list(color = "#e8b34a"),
                hovertemplate = "<b>%{x}</b><br>hours watched: %{y}<extra></extra>") |>
      layout(
        title = list(text = "Movies logged per year & time watched",
                     font = list(color = "#e8e6e0", family = "Space Grotesk")),
        xaxis = list(title = "", tickmode = "linear", gridcolor = "#2a2a2a", tickfont = list(color = "#e8e6e0")),
        yaxis = list(title = "movies logged", gridcolor = "#2a2a2a", tickfont = list(color = "#e8e6e0")),
        yaxis2 = list(title = "hours watched", overlaying = "y", side = "right",
                      tickfont = list(color = "#e8b34a"), gridcolor = "#2a2a2a"),
        legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center", font = list(color = "#e8e6e0")),
        margin = list(b = 70),
        plot_bgcolor = "#1a1a1a", paper_bgcolor = "#1a1a1a"
      ) |> style_plotly()
  })

  # ── Top Movies ──
  output$chart_movies <- renderPlotly({
    df <- filtered_films_df()
    req(!is.null(df), nrow(df) > 0)

    df <- df |>
      arrange(desc(n_watched), desc(avg_rating)) |>
      slice_head(n = 15) |>
      arrange(n_watched)

    df$label <- paste0(df$name, " (", df$year, ")")
    df$label <- factor(df$label, levels = df$label)

    plot_ly(
      df, x = ~n_watched, y = ~label, type = "bar", orientation = "h",
      source = "moviesbar",
      marker = list(color = bar_colors(nrow(df))),
      customdata = ~poster_url,
      hovertemplate = paste0("<b>%{y}</b><br>watched: %{x}<extra></extra>")
    ) |> layout(
      title = list(text = "Your most-watched movies", font = list(color = "#e8e6e0", family = "Space Grotesk")),
      xaxis = list(title = "times watched", gridcolor = "#2a2a2a", tickfont = list(color = "#e8e6e0")),
      yaxis = list(title = "", tickfont = list(color = "#e8e6e0")),
      plot_bgcolor = "#1a1a1a", paper_bgcolor = "#1a1a1a"
    ) |> style_plotly()
  })

  # ── Top Directors ──
  directors_df <- reactive({
    df <- filtered_films_df()
    req(!is.null(df), nrow(df) > 0)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      idx <- which(sapply(rv$films, function(x) x$name == r$name && x$year == r$year))[1]
      dirs <- if (!is.null(rv$films[[idx]]$info)) rv$films[[idx]]$info$directors else list()
      if (length(dirs) == 0) return(NULL)
      do.call(rbind, lapply(dirs, function(d) {
        photo <- if (!is.null(d$profile_path) && !is.na(d$profile_path)) paste0(TMDB_IMG, d$profile_path) else NA_character_
        data.frame(director = d$name, n_watched = r$n_watched, photo_url = photo,
                   popularity = d$popularity %||% NA_real_, stringsAsFactors = FALSE)
      }))
    })
    rows <- rows[!sapply(rows, is.null)]
    if (length(rows) == 0) return(data.frame())
    agg <- do.call(rbind, rows) |>
      group_by(director) |>
      summarise(total_watched = sum(n_watched), unique_movies = n(),
                photo_url = first_non_na(photo_url), popularity = first_non_na(popularity),
                .groups = "drop")

    agg$count <- if (isTRUE(input$count_total_watches)) agg$total_watched else agg$unique_movies
    agg |> arrange(desc(count))
  })

  output$chart_directors <- renderPlotly({
    df <- directors_df()
    req(!is.null(df), nrow(df) > 0)
    df <- df |> slice_head(n = 15) |> arrange(count)
    df$director <- factor(df$director, levels = df$director)

    axis_label <- if (isTRUE(input$count_total_watches)) "total watches" else "unique movies"

    plot_ly(
      df, x = ~count, y = ~director, type = "bar", orientation = "h",
      source = "directorsbar",
      marker = list(color = bar_colors(nrow(df))),
      customdata = ~photo_url,
      hovertemplate = paste0("<b>%{y}</b><br>", axis_label, ": %{x}<extra></extra>")
    ) |> layout(
      title = list(text = "Your most-watched directors", font = list(color = "#e8e6e0", family = "Space Grotesk")),
      xaxis = list(title = axis_label, gridcolor = "#2a2a2a", tickfont = list(color = "#e8e6e0")),
      yaxis = list(title = "", tickfont = list(color = "#e8e6e0")),
      plot_bgcolor = "#1a1a1a", paper_bgcolor = "#1a1a1a"
    ) |> style_plotly()
  })

  # ── Top Actors ──
  actors_df <- reactive({
    df <- filtered_films_df()
    req(!is.null(df), nrow(df) > 0)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      idx <- which(sapply(rv$films, function(x) x$name == r$name && x$year == r$year))[1]
      cast <- if (!is.null(rv$films[[idx]]$info)) rv$films[[idx]]$info$cast else list()
      if (length(cast) == 0) return(NULL)
      do.call(rbind, lapply(cast, function(c) {
        photo <- if (!is.null(c$profile_path) && !is.na(c$profile_path)) paste0(TMDB_IMG, c$profile_path) else NA_character_
        data.frame(actor = c$name, n_watched = r$n_watched, photo_url = photo,
                   popularity = c$popularity %||% NA_real_, stringsAsFactors = FALSE)
      }))
    })
    rows <- rows[!sapply(rows, is.null)]
    if (length(rows) == 0) return(data.frame())
    agg <- do.call(rbind, rows) |>
      group_by(actor) |>
      summarise(total_watched = sum(n_watched), unique_movies = n(),
                photo_url = first_non_na(photo_url), popularity = first_non_na(popularity),
                .groups = "drop")

    agg$count <- if (isTRUE(input$count_total_watches)) agg$total_watched else agg$unique_movies
    agg |> arrange(desc(count))
  })

  output$chart_actors <- renderPlotly({
    df <- actors_df()
    req(!is.null(df), nrow(df) > 0)
    df <- df |> slice_head(n = 15) |> arrange(count)
    df$actor <- factor(df$actor, levels = df$actor)

    axis_label <- if (isTRUE(input$count_total_watches)) "total watches" else "unique movies"

    plot_ly(
      df, x = ~count, y = ~actor, type = "bar", orientation = "h",
      source = "actorsbar",
      marker = list(color = bar_colors(nrow(df))),
      customdata = ~photo_url,
      hovertemplate = paste0("<b>%{y}</b><br>", axis_label, ": %{x}<extra></extra>")
    ) |> layout(
      title = list(text = "Your most-watched actors", font = list(color = "#e8e6e0", family = "Space Grotesk")),
      xaxis = list(title = axis_label, gridcolor = "#2a2a2a", tickfont = list(color = "#e8e6e0")),
      yaxis = list(title = "", tickfont = list(color = "#e8e6e0")),
      plot_bgcolor = "#1a1a1a", paper_bgcolor = "#1a1a1a"
    ) |> style_plotly()
  })

  # ── Top Genres ──
  genre_posters <- reactive({
    req(!is.null(rv$films))
    build_field_posters(rv$films, rv$diary_raw, "genres")
  })

  genres_df <- reactive({
    df <- filtered_films_df()
    req(!is.null(df), nrow(df) > 0)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      idx <- which(sapply(rv$films, function(x) x$name == r$name && x$year == r$year))[1]
      gns <- if (!is.null(rv$films[[idx]]$info)) rv$films[[idx]]$info$genres else character(0)
      if (length(gns) == 0) return(NULL)
      data.frame(genre = gns, n_watched = r$n_watched, stringsAsFactors = FALSE)
    })
    rows <- rows[!sapply(rows, is.null)]
    if (length(rows) == 0) return(data.frame())
    agg <- do.call(rbind, rows) |>
      group_by(genre) |>
      summarise(total_watched = sum(n_watched), unique_movies = n(), .groups = "drop")

    agg$count <- if (isTRUE(input$count_total_watches)) agg$total_watched else agg$unique_movies
    agg |> arrange(desc(count))
  })

  output$chart_genres <- renderPlotly({
    df <- genres_df()
    req(!is.null(df), nrow(df) > 0)
    df <- df |> slice_head(n = 15) |> arrange(count)
    df$genre <- factor(df$genre, levels = df$genre)

    axis_label <- if (isTRUE(input$count_total_watches)) "total watches" else "unique movies"

    plot_ly(
      df, x = ~count, y = ~genre, type = "bar", orientation = "h",
      source = "genresbar",
      marker = list(color = bar_colors(nrow(df))),
      hovertemplate = paste0("<b>%{y}</b><br>", axis_label, ": %{x}<extra></extra>")
    ) |> layout(
      title = list(text = "Your most-watched genres", font = list(color = "#e8e6e0", family = "Space Grotesk")),
      xaxis = list(title = axis_label, gridcolor = "#2a2a2a", tickfont = list(color = "#e8e6e0")),
      yaxis = list(title = "", tickfont = list(color = "#e8e6e0")),
      plot_bgcolor = "#1a1a1a", paper_bgcolor = "#1a1a1a"
    ) |> style_plotly()
  })

  # ── Top Countries ──
  country_posters <- reactive({
    req(!is.null(rv$films))
    build_field_posters(rv$films, rv$diary_raw, "countries")
  })

  countries_df <- reactive({
    df <- filtered_films_df()
    req(!is.null(df), nrow(df) > 0)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      idx <- which(sapply(rv$films, function(x) x$name == r$name && x$year == r$year))[1]
      cns <- if (!is.na(idx) && !is.null(rv$films[[idx]]$info)) rv$films[[idx]]$info$countries else character(0)
      if (length(cns) == 0) return(NULL)
      data.frame(country = cns, n_watched = r$n_watched, stringsAsFactors = FALSE)
    })
    rows <- rows[!sapply(rows, is.null)]
    if (length(rows) == 0) return(data.frame())
    agg <- do.call(rbind, rows) |>
      group_by(country) |>
      summarise(total_watched = sum(n_watched), unique_movies = n(), .groups = "drop")

    agg$count <- if (isTRUE(input$count_total_watches)) agg$total_watched else agg$unique_movies
    agg |> arrange(desc(count))
  })

  output$chart_countries <- renderPlotly({
    df <- countries_df()
    req(!is.null(df), nrow(df) > 0)
    df <- df |> slice_head(n = 15) |> arrange(count)

    country_names <- as.character(df$country)
    df$country <- factor(country_names, levels = country_names)

    axis_label <- if (isTRUE(input$count_total_watches)) "total watches" else "unique movies"

    plot_ly(
      df, x = ~count, y = ~country, type = "bar", orientation = "h",
      source = "countriesbar",
      marker = list(color = bar_colors(nrow(df))),
      hovertemplate = paste0("<b>%{y}</b><br>", axis_label, ": %{x}<extra></extra>")
    ) |> layout(
      title = list(text = "Your most-watched production countries", font = list(color = "#e8e6e0", family = "Space Grotesk")),
      xaxis = list(title = axis_label, gridcolor = "#2a2a2a", tickfont = list(color = "#e8e6e0")),
      yaxis = list(title = "", tickfont = list(color = "#e8e6e0")),
      plot_bgcolor = "#1a1a1a", paper_bgcolor = "#1a1a1a"
    ) |> style_plotly()
  })

  # ── Genres by Decade (of release year), as % share ──
  # Respects the watched-date filter, same as everything else.
  genre_decade_df <- reactive({
    df <- filtered_films_df()
    req(!is.null(df), nrow(df) > 0)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      if (is.na(r$year)) return(NULL)
      idx <- which(sapply(rv$films, function(x) x$name == r$name && x$year == r$year))[1]
      gns <- if (!is.na(idx) && !is.null(rv$films[[idx]]$info)) rv$films[[idx]]$info$genres else character(0)
      if (length(gns) == 0) return(NULL)
      decade <- (r$year %/% 10) * 10
      data.frame(decade = decade, genre = gns, stringsAsFactors = FALSE)
    })
    rows <- rows[!sapply(rows, is.null)]
    if (length(rows) == 0) return(data.frame())
    do.call(rbind, rows)
  })

  output$chart_genre_decades <- renderPlotly({
    raw <- genre_decade_df()
    req(nrow(raw) > 0)

    top_genres <- raw |> count(genre, sort = TRUE) |> slice_head(n = 11) |> pull(genre)

    df <- raw |>
      mutate(genre = ifelse(genre %in% top_genres, genre, "Other")) |>
      count(decade, genre, name = "n") |>
      group_by(decade) |>
      mutate(pct = n / sum(n) * 100) |>
      ungroup()

    all_genres    <- c(top_genres, "Other")
    genre_colors  <- c(get_genre_colors(length(top_genres)), "#666660")
    decades       <- sort(unique(df$decade))
    decade_labels <- paste0(decades, "s")

    wide <- expand.grid(decade = decades, genre = all_genres, stringsAsFactors = FALSE) |>
      left_join(df |> select(decade, genre, pct), by = c("decade", "genre")) |>
      mutate(pct = ifelse(is.na(pct), 0, pct))

    p <- plot_ly(source = "genredecadebar")
    for (i in seq_along(all_genres)) {
      g <- all_genres[i]
      sub <- wide[wide$genre == g, ]
      sub <- sub[order(sub$decade), ]
      p <- add_trace(p,
        x = paste0(sub$decade, "s"), y = sub$pct, name = g,
        type = "bar",
        marker = list(color = paste0(genre_colors[i], "dd"),
                      line = list(color = "#141414", width = 1)),
        hovertemplate = paste0("<b>", g, "</b>: %{y:.1f}%<extra></extra>")
      )
    }

    p |> layout(
      barmode = "stack",
      bargap = 0.15,
      title = list(text = "Your genre mix by decade (of the films' release year)",
                   font = list(color = "#e8e6e0", family = "Space Grotesk")),
      xaxis = list(title = "decade", categoryorder = "array", categoryarray = decade_labels,
                   gridcolor = "#2a2a2a", tickfont = list(color = "#e8e6e0")),
      yaxis = list(title = "share of movies", ticksuffix = "%", range = c(0, 100),
                   gridcolor = "#2a2a2a", tickfont = list(color = "#666660")),
      hovermode = "closest",
      legend = list(orientation = "v", x = 1.02, y = 1, font = list(color = "#e8e6e0")),
      plot_bgcolor = "#1a1a1a", paper_bgcolor = "#1a1a1a"
    ) |> style_plotly()
  })

  # ── Diary habits: day-of-week x month heatmap (diary-logged movies only) ──
  # Letterboxd only stores a calendar date (no time-of-day, no timezone),
  # so this is based on the "Watched Date" you entered, as-is.
  diary_habits_df <- reactive({
    d <- if (isTRUE(input$use_year_filter)) filtered_diary() else rv$diary_raw
    req(!is.null(d), nrow(d) > 0)
    dates <- suppressWarnings(as.Date(d$watched_date))
    dates <- dates[!is.na(dates)]
    req(length(dates) > 0)

    weekday_num <- (lubridate::wday(dates) + 5) %% 7 + 1  # 1=Mon ... 7=Sun
    day_labels  <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

    data.frame(
      weekday = factor(day_labels[weekday_num], levels = day_labels),
      month   = factor(month.abb[lubridate::month(dates)], levels = month.abb),
      stringsAsFactors = FALSE
    )
  })

  output$chart_diary_habits <- renderPlotly({
    hd <- diary_habits_df()
    req(nrow(hd) > 0)

    day_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

    heat <- hd |> group_by(weekday, month) |> summarise(n = n(), .groups = "drop")
    all_combos <- expand.grid(weekday = day_labels, month = month.abb, stringsAsFactors = FALSE)
    heat <- left_join(all_combos, heat, by = c("weekday", "month")) |>
      mutate(n = ifelse(is.na(n), 0, n),
             weekday = factor(weekday, levels = day_labels),
             month   = factor(month, levels = month.abb),
             tooltip = paste0(weekday, " \u00b7 ", month, "<br>logged: ", n))

    p <- ggplot(heat, aes(x = weekday, y = factor(month, levels = rev(month.abb)), fill = n, text = tooltip)) +
      geom_tile(color = "white", linewidth = 0.4) +
      scale_fill_gradient(low = "#1e1530", high = "#9d7fd4", name = "logged") +
      labs(title = "When you log movies (day of week × month)", x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(size = 14, color = "#e8e6e0", family = "Space Grotesk"),
        axis.text        = element_text(color = "#e8e6e0", family = "Space Grotesk"),
        panel.grid       = element_blank(),
        legend.text      = element_text(color = "#999993", family = "Space Grotesk"),
        legend.title     = element_text(color = "#999993", family = "Space Grotesk"),
        plot.background  = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#1a1a1a", color = NA)
      )
    ggplotly(p, tooltip = "text") |> style_plotly()
  })

  # ── Poster hover panel (shared across the charts with photos) ──
  show_poster_hover <- function(ed) {
    poster <- ed$customdata
    label  <- ed$y

    if (is.null(poster) || is.na(poster)) {
      shinyjs::runjs("document.getElementById('poster-panel').style.display='none';")
      return()
    }

    html <- paste0(
      "<span class='p-close' onclick='hidePosterPanel()'>✕</span>",
      "<img src='", poster, "'/>",
      "<div class='p-title'>", htmltools::htmlEscape(label), "</div>"
    )
    shinyjs::runjs(paste0(
      "var el = document.getElementById('poster-panel');",
      "el.innerHTML = ", shQuote(html), ";",
      "el.style.display = 'block';"
    ))
  }

  observeEvent(event_data("plotly_hover", source = "moviesbar"), {
    show_poster_hover(event_data("plotly_hover", source = "moviesbar"))
  })
  observeEvent(event_data("plotly_hover", source = "directorsbar"), {
    show_poster_hover(event_data("plotly_hover", source = "directorsbar"))
  })
  observeEvent(event_data("plotly_hover", source = "actorsbar"), {
    show_poster_hover(event_data("plotly_hover", source = "actorsbar"))
  })

  # Tap support for touch devices (hover doesn't exist on mobile)
  observeEvent(event_data("plotly_click", source = "moviesbar"), {
    show_poster_hover(event_data("plotly_click", source = "moviesbar"))
  })
  observeEvent(event_data("plotly_click", source = "directorsbar"), {
    show_poster_hover(event_data("plotly_click", source = "directorsbar"))
  })
  observeEvent(event_data("plotly_click", source = "actorsbar"), {
    show_poster_hover(event_data("plotly_click", source = "actorsbar"))
  })

  observeEvent(event_data("plotly_unhover", source = "moviesbar"), {
    shinyjs::runjs("document.getElementById('poster-panel').style.display='none';")
  }, ignoreNULL = FALSE)
  observeEvent(event_data("plotly_unhover", source = "directorsbar"), {
    shinyjs::runjs("document.getElementById('poster-panel').style.display='none';")
  }, ignoreNULL = FALSE)
  observeEvent(event_data("plotly_unhover", source = "actorsbar"), {
    shinyjs::runjs("document.getElementById('poster-panel').style.display='none';")
  }, ignoreNULL = FALSE)

  # ── Collage hover (2x2 posters), shared by Genres and Countries ──
  show_collage_hover <- function(label, imgs) {
    imgs <- imgs[!is.na(imgs)]
    if (is.null(imgs) || length(imgs) == 0) {
      shinyjs::runjs("document.getElementById('poster-panel').style.display='none';")
      return()
    }
    img_tags <- paste0("<img src='", imgs, "'/>", collapse = "")
    html <- paste0(
      "<span class='p-close' onclick='hidePosterPanel()'>✕</span>",
      "<div class='poster-collage'>", img_tags, "</div>",
      "<div class='p-title'>", htmltools::htmlEscape(label), "</div>"
    )
    shinyjs::runjs(paste0(
      "var el = document.getElementById('poster-panel');",
      "el.innerHTML = ", shQuote(html), ";",
      "el.style.display = 'block';"
    ))
  }

  observeEvent(event_data("plotly_hover", source = "genresbar"), {
    ed <- event_data("plotly_hover", source = "genresbar")
    show_collage_hover(ed$y, genre_posters()[[ed$y]])
  })
  observeEvent(event_data("plotly_click", source = "genresbar"), {
    ed <- event_data("plotly_click", source = "genresbar")
    show_collage_hover(ed$y, genre_posters()[[ed$y]])
  })
  observeEvent(event_data("plotly_unhover", source = "genresbar"), {
    shinyjs::runjs("document.getElementById('poster-panel').style.display='none';")
  }, ignoreNULL = FALSE)

  observeEvent(event_data("plotly_hover", source = "countriesbar"), {
    ed <- event_data("plotly_hover", source = "countriesbar")
    show_collage_hover(ed$y, country_posters()[[ed$y]])
  })
  observeEvent(event_data("plotly_click", source = "countriesbar"), {
    ed <- event_data("plotly_click", source = "countriesbar")
    show_collage_hover(ed$y, country_posters()[[ed$y]])
  })
  observeEvent(event_data("plotly_unhover", source = "countriesbar"), {
    shinyjs::runjs("document.getElementById('poster-panel').style.display='none';")
  }, ignoreNULL = FALSE)

  # ── Highlight the hovered genre across all decade bars ──
  observeEvent(event_data("plotly_hover", source = "genredecadebar"), {
    ed <- event_data("plotly_hover", source = "genredecadebar")
    curve <- ed$curveNumber
    shinyjs::runjs(paste0(
      "var gd = document.getElementById('chart_genre_decades');",
      "if (gd && gd.data) {",
        "var n = gd.data.length;",
        "var ops = [];",
        "for (var i = 0; i < n; i++) ops.push(i === ", curve, " ? 1 : 0.45);",
        "Plotly.restyle(gd, {opacity: ops});",
      "}"
    ))
  })

  observeEvent(event_data("plotly_unhover", source = "genredecadebar"), {
    shinyjs::runjs(paste0(
      "var gd = document.getElementById('chart_genre_decades');",
      "if (gd && gd.data) {",
        "var n = gd.data.length;",
        "var ops = [];",
        "for (var i = 0; i < n; i++) ops.push(1);",
        "Plotly.restyle(gd, {opacity: ops});",
      "}"
    ))
  }, ignoreNULL = FALSE)
}

shinyApp(ui, server)
