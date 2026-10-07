# Hitting Report App — single-file Shiny app
# Select CSV + team + date + hitter; preview and download PDF.

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)

# ---------- App home / CSV helpers ----------
HITTER_APP_HOME <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  }
  wd <- normalizePath(getwd(), winslash = "/")
  if (file.exists(file.path(wd, "app.R"))) return(wd)
  if (file.exists(file.path(wd, "Hitting Report App.R"))) return(wd)
  wd
})

app_dir <- function() HITTER_APP_HOME

list_local_csvs <- function(dir = app_dir()) {
  files <- list.files(dir, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
  if (length(files) == 0) return(character(0))
  files[order(basename(files))]
}

has_csv_upload <- function(upload) {
  !is.null(upload) &&
    !is.null(upload$datapath) &&
    length(upload$datapath) > 0 &&
    nzchar(upload$datapath[1]) &&
    file.exists(upload$datapath[1])
}

GAME_DATE_ALL <- "All"

is_all_dates_selected <- function(selected) {
  if (is.null(selected) || length(selected) == 0) {
    return(FALSE)
  }
  GAME_DATE_ALL %in% as.character(selected)
}

game_date_choices <- function(dates) {
  dates <- sort(unique(dates[!is.na(dates)]))
  if (length(dates) == 0) {
    return(character(0))
  }
  labels <- format(dates, "%m/%d/%Y")
  values <- format(dates, "%Y-%m-%d")
  names(values) <- labels
  c("All" = GAME_DATE_ALL, values)
}

coerce_selected_game_dates <- function(selected, available_dates) {
  available_dates <- sort(unique(available_dates[!is.na(available_dates)]))
  if (is_all_dates_selected(selected)) {
    return(available_dates)
  }
  if (is.null(selected) || length(selected) == 0) {
    return(as.Date(character(0)))
  }
  sel <- selected[as.character(selected) != GAME_DATE_ALL]
  if (length(sel) == 0) {
    return(as.Date(character(0)))
  }
  parsed <- safe_date(sel)
  intersect(parsed[!is.na(parsed)], available_dates)
}

resolve_selected_dates <- function(selected, available_dates) {
  coerce_selected_game_dates(selected, available_dates)
}

filter_df_by_game_dates <- function(df, selected, available_dates = NULL) {
  if (is.null(available_dates)) {
    available_dates <- df$Date
  }
  dates <- resolve_selected_dates(selected, available_dates)
  if (length(dates) == 0) {
    return(df[FALSE, , drop = FALSE])
  }
  df %>% filter(Date %in% dates)
}

preserve_game_date_selection <- function(current, available_dates) {
  available_dates <- sort(unique(available_dates[!is.na(available_dates)]))
  if (length(available_dates) == 0) {
    return(character(0))
  }
  if (is_all_dates_selected(current)) {
    return(GAME_DATE_ALL)
  }
  if (is.null(current) || length(current) == 0) {
    return(format(available_dates[1], "%Y-%m-%d"))
  }
  valid <- coerce_selected_game_dates(current, available_dates)
  if (length(valid) > 0) {
    return(format(valid, "%Y-%m-%d"))
  }
  format(available_dates[1], "%Y-%m-%d")
}

game_date_status_text <- function(selected) {
  if (is_all_dates_selected(selected)) {
    return("All dates")
  }
  sel <- selected[as.character(selected) != GAME_DATE_ALL]
  date_vec <- safe_date(sel)
  date_vec <- date_vec[!is.na(date_vec)]
  if (length(date_vec) == 1) {
    return(format(date_vec[1], "%B %d, %Y"))
  }
  "Multiple dates"
}

# ---------- Report logic (adapted from Hitter Synergy Report.R) ----------
sz_xmin <- -0.83
sz_xmax <- 0.83
sz_ymin <- 1.479
sz_ymax <- 3.5
sz_x_breaks <- seq(sz_xmin, sz_xmax, length.out = 4)
sz_y_breaks <- seq(sz_ymin, sz_ymax, length.out = 4)
location_coord_ratio <- 1.22
location_ymin <- 1
location_ymax <- 4.05

pitch_types_fastball <- c("Fastball", "Sinker", "FourSeamFastBall")
pitch_types_breaking <- c("Slider", "Cutter", "Curveball", "Sweeper")
pitch_types_offspeed <- c("ChangeUp", "Splitter")

pitch_type_colors <- c(
  "Fastball" = "#B22222",
  "FourSeamFastBall" = "#B22222",
  "Sinker" = "#CD6600",
  "Cutter" = "#2F4F4F",
  "Slider" = "#E6C200",
  "Sweeper" = "#DAA520",
  "Curveball" = "#5CACEE",
  "ChangeUp" = "#458B00",
  "Splitter" = "#6B8E23",
  "Undefined" = "#888888"
)

play_result_colors <- c(
  "Single" = "#2563eb",
  "Double" = "#dc2626",
  "Triple" = "#ca8a04",
  "HomeRun" = "#16a34a",
  "Out" = "#1f2937"
)

report_header_navy <- "navyblue"
hitting_table_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 7.5, fontface = "plain", fontfamily = "sans"),
    bg_params = list(fill = "white", col = "grey80"),
    padding = unit(c(2, 2.5), "mm")
  ),
  colhead = list(
    fg_params = list(fontsize = 7.5, fontface = "bold", col = "white"),
    bg_params = list(fill = report_header_navy, col = "grey75"),
    padding = unit(c(1.5, 2.5), "mm")
  )
)

strike_zone_nine_box_layers <- list(
  geom_segment(x = sz_x_breaks[2], xend = sz_x_breaks[2], y = sz_ymin, yend = sz_ymax,
               inherit.aes = FALSE, color = "grey35", linewidth = 0.35),
  geom_segment(x = sz_x_breaks[3], xend = sz_x_breaks[3], y = sz_ymin, yend = sz_ymax,
               inherit.aes = FALSE, color = "grey35", linewidth = 0.35),
  geom_segment(x = sz_xmin, xend = sz_xmax, y = sz_y_breaks[2], yend = sz_y_breaks[2],
               inherit.aes = FALSE, color = "grey35", linewidth = 0.35),
  geom_segment(x = sz_xmin, xend = sz_xmax, y = sz_y_breaks[3], yend = sz_y_breaks[3],
               inherit.aes = FALSE, color = "grey35", linewidth = 0.35)
)

safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
  # Mixed formats are common (e.g. some rows "2026-03-25" and others "2/19/26").
  # Parse iteratively so early matches don't prevent later formats.
  out <- rep(as.Date(NA), length(x_chr))
  remaining <- is.na(out) & !is.na(x_chr)
  for (fmt in c("%Y-%m-%d", "%m/%d/%y", "%m/%d/%Y", "%m-%d-%Y", "%m-%d-%y", "%Y%m%d")) {
    if (!any(remaining)) break
    parsed <- as.Date(x_chr[remaining], format = fmt)
    ok <- !is.na(parsed)
    if (any(ok)) {
      idx <- which(remaining)[ok]
      out[idx] <- parsed[ok]
      remaining[idx] <- FALSE
    }
  }
  # Last resort
  if (any(remaining)) {
    parsed <- suppressWarnings(as.Date(x_chr[remaining]))
    out[which(remaining)] <- parsed
  }
  out
}

prepare_hitter_data <- function(data) {
  if (!"Date" %in% names(data) && "GameDate" %in% names(data)) {
    data$Date <- data$GameDate
  }
  if ("Date" %in% names(data)) {
    data$Date <- safe_date(data$Date)
  }
  if ("TaggedPitchType" %in% names(data)) {
    data <- data %>%
      filter(TaggedPitchType != "Undefined") %>%
      mutate(
        TaggedPitchType = case_when(
          TaggedPitchType %in% c("FourSeamFastBall", "Sinker") ~ "Fastball",
          TaggedPitchType == "Splitter" ~ "ChangeUp",
          TaggedPitchType == "Sweeper" ~ "Slider",
          TRUE ~ TaggedPitchType
        )
      )
  }
  data
}

format_hitter_name <- function(batter) {
  parts <- strsplit(batter, ", ", fixed = TRUE)[[1]]
  if (length(parts) >= 2) paste(parts[2], parts[1]) else batter
}

attach_title_to_table_grob <- function(tg, label) {
  tit <- textGrob(
    label,
    x = unit(0.5, "npc"),
    y = unit(0.45, "npc"),
    just = "center",
    gp = gpar(fontsize = 10, fontface = "bold", col = "black", fontfamily = "sans")
  )
  tit_h <- grobHeight(tit) + unit(4, "pt")
  nc <- length(tg$widths)
  tg2 <- gtable::gtable_add_rows(tg, heights = tit_h, pos = 0)
  gtable::gtable_add_grob(tg2, tit, t = 1, l = 1, r = nc, b = 1, z = Inf, clip = "off")
}

table_with_title <- function(tbl, title) {
  tg <- tableGrob(tbl, rows = NULL, theme = hitting_table_theme)
  attach_title_to_table_grob(tg, title)
}

add_hitter_pitch_flags <- function(df) {
  df %>%
    mutate(
      Strikes_pre = suppressWarnings(as.integer(Strikes)),
      Balls_pre = suppressWarnings(as.integer(Balls)),
      InStrikeZone = PlateLocSide >= sz_xmin & PlateLocSide <= sz_xmax &
        PlateLocHeight >= sz_ymin & PlateLocHeight <= sz_ymax,
      Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
      Chase = ifelse(!InStrikeZone & Swing, 1L, 0L),
      ZSwing = ifelse(InStrikeZone & Swing, 1L, 0L),
      Whiff = PitchCall == "StrikeSwinging",
      ZWhiff = PitchCall == "StrikeSwinging" & InStrikeZone,
      StrikeTaken = PitchCall == "StrikeCalled",
      HardHitCheck = ifelse(!is.na(ExitSpeed) & ExitSpeed >= 95 & ExitSpeed <= 120 & PitchCall == "InPlay", 1L, 0L),
      PitchBucket = case_when(
        TaggedPitchType %in% pitch_types_fastball ~ "Fastballs",
        TaggedPitchType %in% pitch_types_breaking ~ "Breaking Balls",
        TaggedPitchType %in% pitch_types_offspeed ~ "Offspeed",
        TRUE ~ "Other"
      )
    )
}

strike_zone_location_plot <- function(df, title) {
  sz_scales <- list(
    scale_x_continuous(limits = c(-1.8, 1.8), expand = c(0, 0)),
    scale_y_continuous(limits = c(location_ymin, location_ymax), expand = c(0, 0)),
    coord_fixed(ratio = location_coord_ratio, expand = FALSE, clip = "off")
  )
  sz_theme <- theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 8, hjust = 0.5, margin = margin(b = 2))
  )
  zone_outline <- geom_rect(
    xmin = sz_xmin, xmax = sz_xmax, ymin = sz_ymin, ymax = sz_ymax,
    inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.6
  )
  if (nrow(df) == 0) {
    return(
      ggplot() + zone_outline + strike_zone_nine_box_layers +
        annotate("text", x = 0, y = 2.5, label = "No pitches", size = 3) +
        labs(title = title, x = NULL, y = NULL) +
        theme_minimal() + sz_theme + sz_scales +
        theme(legend.position = "none")
    )
  }
  ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)) +
    zone_outline + strike_zone_nine_box_layers +
    geom_point(shape = 21, size = 2, stroke = 0.3, color = "black", alpha = 0.8, na.rm = TRUE) +
    scale_fill_manual(values = pitch_type_colors, drop = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() + sz_theme + sz_scales +
    theme(legend.position = "none") + guides(fill = "none")
}

safe_pct <- function(num, den, digits = 1) {
  if (length(den) == 0 || is.na(den) || den == 0) return(0)
  round(100 * num / den, digits)
}
safe_rate <- function(num, den, digits = 3) {
  if (length(den) == 0 || is.na(den) || den == 0) return(0)
  round(num / den, digits)
}

spray_slice <- function(bearing, batter_side) {
  if (is.na(bearing)) return(NA_character_)
  if (identical(batter_side, "Right")) {
    if (bearing < -15) return("PULL")
    if (bearing > 15) return("OPPO")
    return("MIDDLE")
  }
  if (identical(batter_side, "Left")) {
    if (bearing > 15) return("PULL")
    if (bearing < -15) return("OPPO")
    return("MIDDLE")
  }
  "MIDDLE"
}

build_overall_table <- function(df) {
  df %>%
    summarise(
      PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk")),
      AB = sum((PitchCall %in% c("InPlay") | KorBB == "Strikeout") & !PlayResult %in% c("Sacrifice")),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
      `1B` = sum(PlayResult == "Single"),
      `2B` = sum(PlayResult == "Double"),
      `3B` = sum(PlayResult == "Triple"),
      HR = sum(PlayResult == "HomeRun"),
      K = sum(KorBB == "Strikeout"),
      BB = sum(KorBB == "Walk"),
      HBP = sum(PitchCall == "HitByPitch"),
      BA = safe_rate(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
                     sum((PitchCall %in% c("InPlay") | KorBB == "Strikeout") & !PlayResult %in% c("Sacrifice"))),
      OBP = safe_rate(
        sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) + sum(KorBB == "Walk") + sum(PitchCall == "HitByPitch"),
        sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"))
      ),
      SLG = safe_rate(
        sum(PlayResult == "Single") + 2 * sum(PlayResult == "Double") + 3 * sum(PlayResult == "Triple") + 4 * sum(PlayResult == "HomeRun"),
        sum((PitchCall %in% c("InPlay") | KorBB == "Strikeout") & !PlayResult %in% c("Sacrifice"))
      )
    ) %>%
    mutate(OPS = round(OBP + SLG, 3))
}

build_spray_exit_table <- function(df) {
  bip <- df %>% filter(PitchCall == "InPlay")
  bbe <- nrow(bip)
  swings <- sum(df$Swing, na.rm = TRUE)
  gb <- sum(bip$TaggedHitType == "GroundBall", na.rm = TRUE)
  fb <- sum(bip$TaggedHitType %in% c("FlyBall", "Popup"), na.rm = TRUE)
  ld <- sum(bip$TaggedHitType == "LineDrive", na.rm = TRUE)
  bip_types <- sum(bip$TaggedHitType %in% c("GroundBall", "FlyBall", "LineDrive", "Popup"), na.rm = TRUE)
  side <- unique(stats::na.omit(df$BatterSide))[1]
  bearings <- bip$Bearing[!is.na(bip$Bearing)]
  slices <- vapply(bearings, spray_slice, character(1), batter_side = side)
  pull_n <- sum(slices == "PULL", na.rm = TRUE)
  mid_n <- sum(slices == "MIDDLE", na.rm = TRUE)
  oppo_n <- sum(slices == "OPPO", na.rm = TRUE)

  tibble::tibble(
    BBE = as.character(bbe),
    `BIP%` = paste0(safe_pct(bbe, swings), "%"),
    `GB%` = paste0(safe_pct(gb, bip_types), "%"),
    `FB%` = paste0(safe_pct(fb, bip_types), "%"),
    `LD%` = paste0(safe_pct(ld, bip_types), "%"),
    PULL = paste0(safe_pct(pull_n, bbe), "%"),
    MIDDLE = paste0(safe_pct(mid_n, bbe), "%"),
    OPPO = paste0(safe_pct(oppo_n, bbe), "%"),
    EV = if (bbe == 0) "—" else as.character(round(mean(bip$ExitSpeed, na.rm = TRUE), 1)),
    `MAX EV` = if (bbe == 0) "—" else as.character(round(max(bip$ExitSpeed, na.rm = TRUE), 1)),
    `HH%` = paste0(safe_pct(sum(bip$ExitSpeed >= 95 & bip$ExitSpeed <= 120, na.rm = TRUE), bbe), "%")
  )
}

summarise_pitch_bucket <- function(df, label) {
  empty_row <- tibble::tibble(
    `Pitch Type` = label, AB = 0L, H = 0L, BA = 0,
    `SWING%` = 0, `Z-SWING%` = 0, `WHIFF%` = 0, `Z-WHIFF%` = 0,
    `CHASE%` = 0, `Z-CONTACT%` = 0
  )
  if (nrow(df) == 0) return(empty_row)

  pitches <- nrow(df)
  ab <- sum(
    (df$PitchCall %in% c("InPlay") | df$KorBB %in% c("Strikeout")) &
      !df$PitchCall %in% c("HitByPitch") &
      !df$KorBB %in% c("Walk") &
      !df$PlayResult %in% c("Sacrifice"),
    na.rm = TRUE
  )
  h <- sum(df$PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE)
  swings <- sum(df$Swing, na.rm = TRUE)
  z_swings <- sum(df$ZSwing, na.rm = TRUE)
  in_zone <- sum(df$InStrikeZone, na.rm = TRUE)
  whiffs <- sum(df$PitchCall == "StrikeSwinging", na.rm = TRUE)
  z_whiffs <- sum(df$PitchCall == "StrikeSwinging" & df$InStrikeZone, na.rm = TRUE)
  chases <- sum(df$Chase, na.rm = TRUE)

  tibble::tibble(
    `Pitch Type` = label,
    AB = ab,
    H = h,
    BA = safe_rate(h, ab),
    `SWING%` = safe_pct(swings, pitches),
    `Z-SWING%` = safe_pct(z_swings, in_zone),
    `WHIFF%` = safe_pct(whiffs, swings),
    `Z-WHIFF%` = safe_pct(z_whiffs, z_swings),
    `CHASE%` = safe_pct(chases, swings),
    `Z-CONTACT%` = safe_pct(z_swings - z_whiffs, z_swings)
  )
}

build_stats_by_pitch_type_table <- function(df) {
  bind_rows(
    summarise_pitch_bucket(df %>% filter(PitchBucket == "Fastballs"), "Fastballs"),
    summarise_pitch_bucket(df %>% filter(PitchBucket == "Breaking Balls"), "Breaking Balls"),
    summarise_pitch_bucket(df %>% filter(PitchBucket == "Offspeed"), "Offspeed"),
    summarise_pitch_bucket(df, "Overall")
  )
}

pitch_pct_row <- function(df, label) {
  n <- nrow(df)
  if (n == 0) {
    return(tibble::tibble(Count = label, `FASTBALLS%` = 0, `BREAKING%` = 0, `OFFSPEED%` = 0))
  }
  tibble::tibble(
    Count = label,
    `FASTBALLS%` = paste0(safe_pct(sum(df$PitchBucket == "Fastballs"), n), "%"),
    `BREAKING%` = paste0(safe_pct(sum(df$PitchBucket == "Breaking Balls"), n), "%"),
    `OFFSPEED%` = paste0(safe_pct(sum(df$PitchBucket == "Offspeed"), n), "%")
  )
}

build_pitch_pct_table <- function(df) {
  df <- df %>%
    mutate(
      CountGroup = case_when(
        Balls_pre == 0 & Strikes_pre == 0 ~ "0-0",
        Balls_pre > Strikes_pre ~ "Ahead",
        Strikes_pre > Balls_pre ~ "Behind",
        TRUE ~ "Even"
      )
    )
  bind_rows(
    pitch_pct_row(df, "All Counts"),
    pitch_pct_row(df %>% filter(CountGroup == "0-0"), "0-0"),
    pitch_pct_row(df %>% filter(CountGroup == "Ahead"), "Ahead"),
    pitch_pct_row(df %>% filter(CountGroup == "Behind"), "Behind")
  )
}

build_spray_chart <- function(df) {
  spray <- df %>%
    filter(
      PitchCall == "InPlay",
      PlayResult %in% names(play_result_colors),
      !is.na(Bearing), !is.na(Distance)
    ) %>%
    mutate(
      hc_x = sin(Bearing * pi / 180) * Distance,
      hc_y = cos(Bearing * pi / 180) * Distance,
      BattedBallType = dplyr::case_when(
        TaggedHitType %in% c("GroundBall") ~ "GroundBall",
        TaggedHitType %in% c("LineDrive") ~ "LineDrive",
        TaggedHitType %in% c("FlyBall") ~ "FlyBall",
        TaggedHitType %in% c("Popup") ~ "Popup",
        TRUE ~ NA_character_
      )
    )

  linetype_map <- c("GroundBall" = "dotted", "LineDrive" = "solid", "FlyBall" = "longdash", "Popup" = "dotdash")

  p <- ggplot(spray, aes(x = hc_x, y = hc_y)) +
    theme_void() +
    labs(title = "OVERALL") +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 7, face = "bold"),
      legend.text = element_text(size = 6),
      legend.margin = margin(t = 1, b = 0),
      legend.box.spacing = unit(0, "pt")
    )
  if (requireNamespace("GeomMLBStadiums", quietly = TRUE)) {
    p <- p + GeomMLBStadiums::geom_mlb_stadium(
      stadium_ids = "dodgers",
      stadium_transform_coords = TRUE,
      stadium_segments = "all",
      linewidth = 0.4,
      color = "black"
    )
  }
  if (nrow(spray) > 0) {
    p <- p +
      geom_segment(
        data = spray %>% filter(!is.na(BattedBallType)),
        aes(x = 0, y = 0, xend = hc_x, yend = hc_y, linetype = BattedBallType),
        inherit.aes = FALSE,
        color = "black",
        linewidth = 0.35,
        alpha = 0.55
      ) +
      geom_point(aes(fill = PlayResult), shape = 21, colour = "black", size = 2.5, alpha = 0.85) +
      scale_fill_manual(values = play_result_colors, name = "Result") +
      scale_linetype_manual(values = linetype_map, name = "Batted ball") +
      coord_fixed()
  }
  p
}

build_header_grob <- function(batter, df, date_label) {
  formatted <- format_hitter_name(batter)
  team <- unique(stats::na.omit(df$BatterTeam))[1]
  player_line <- paste0(formatted, "  |  ", team)
  title_line <- "HITTING REPORT"
  top_block <- grobTree(
    rectGrob(gp = gpar(fill = "#f0f4f0", col = NA)),
    textGrob(player_line, y = unit(0.78, "npc"), gp = gpar(fontsize = 11, fontface = "bold")),
    textGrob(title_line, y = unit(0.48, "npc"), gp = gpar(fontsize = 13, fontface = "bold")),
    textGrob(date_label, y = unit(0.22, "npc"), gp = gpar(fontsize = 9, fontface = "italic"))
  )
  top_block
}

build_hitting_report_pdf <- function(file, hitter_data, batter) {
  if (nrow(hitter_data) == 0) stop("No rows for this hitter.")
  df <- add_hitter_pitch_flags(prepare_hitter_data(hitter_data))

  dates <- sort(unique(df$Date[!is.na(df$Date)]))
  date_label <- if (length(dates) == 0) {
    "Date range N/A"
  } else if (length(dates) == 1) {
    format(dates[1], "%m/%d/%Y")
  } else {
    "" # multiple dates selected: omit date from header
  }

  overall_tbl <- build_overall_table(df)
  spray_tbl <- build_spray_exit_table(df)
  pitch_type_tbl <- build_stats_by_pitch_type_table(df)
  pitch_pct_tbl <- build_pitch_pct_table(df)

  loc_hits <- strike_zone_location_plot(df %>% filter(PitchCall == "InPlay", PlayResult %in% names(play_result_colors)), "HITS")
  loc_outs <- strike_zone_location_plot(df %>% filter(PitchCall == "InPlay", PlayResult == "Out"), "OUTS")
  loc_all <- strike_zone_location_plot(df, "ALL PITCHES")
  loc_fb <- strike_zone_location_plot(df %>% filter(PitchBucket == "Fastballs"), "FB")
  loc_bb <- strike_zone_location_plot(df %>% filter(PitchBucket == "Breaking Balls"), "BB")
  loc_os <- strike_zone_location_plot(df %>% filter(PitchBucket == "Offspeed"), "OS")
  loc_swings <- strike_zone_location_plot(df %>% filter(Swing == TRUE), "SWINGS")
  loc_whiffs <- strike_zone_location_plot(df %>% filter(Whiff == TRUE), "WHIFFS")
  loc_chase <- strike_zone_location_plot(df %>% filter(Chase == 1L), "CHASE")
  loc_taken <- strike_zone_location_plot(df %>% filter(StrikeTaken == TRUE), "STRIKES TAKEN")

  spray_chart <- build_spray_chart(df)

  left_panel <- arrangeGrob(
    table_with_title(overall_tbl, "OVERALL"),
    table_with_title(spray_tbl, "SPRAY TABLE & EXIT DATA"),
    table_with_title(pitch_type_tbl, "STATS BY PITCH TYPE"),
    table_with_title(pitch_pct_tbl, "PITCH %"),
    nrow = 4,
    heights = unit(c(1.02, 1.1, 1.32, 1.02), "null")
  )

  outcomes_row <- arrangeGrob(loc_hits, loc_outs, ncol = 2)
  pitch_dist_row <- arrangeGrob(loc_all, loc_fb, loc_bb, loc_os, ncol = 4)
  discipline_row <- arrangeGrob(loc_swings, loc_whiffs, loc_chase, loc_taken, ncol = 4)

  right_panel <- arrangeGrob(
    outcomes_row,
    pitch_dist_row,
    discipline_row,
    spray_chart,
    nrow = 4,
    heights = unit(c(1, 1, 1, 1.35), "null")
  )

  header <- build_header_grob(batter, df, date_label)

  page <- arrangeGrob(
    header,
    arrangeGrob(left_panel, right_panel, ncol = 2, widths = unit(c(0.40, 0.60), "npc")),
    nrow = 2,
    heights = unit(c(0.11, 0.89), "npc")
  )

  pdf(file, width = 14, height = 8.5, useDingbats = FALSE)
  on.exit(dev.off(), add = TRUE)
  grid.newpage()
  grid.draw(page)
  invisible(file)
}

# ---------- UI ----------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      :root {
        --bg: #f5f7ff;
        --surface: #ffffff;
        --surface-2: #f3f6ff;
        --text: #0f172a;
        --muted: #475569;
        --border: rgba(15, 23, 42, 0.12);
        --navy: #1e3a8a;
        --navy-dark: #172554;
        --shadow: 0 8px 30px rgba(15, 23, 42, 0.08);
      }
      body { background: var(--bg); color: var(--text); font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; margin: 0; }
      .app-wrap { max-width: 1480px; margin: 0 auto; padding: 0 18px 28px; }
      .hero {
        background: linear-gradient(135deg, var(--navy-dark) 0%, var(--navy) 55%, #2563eb 100%);
        border-radius: 18px;
        padding: 28px 32px 24px;
        margin: 18px 0 20px;
        box-shadow: var(--shadow);
        position: relative;
        overflow: hidden;
      }
      .hero h1 { font-size: 52px; letter-spacing: 0.03em; color: #fff; margin: 0 0 4px; line-height: 1; }
      .hero p { color: #dbeafe; margin: 0; font-size: 15px; font-weight: 500; }
      .hero-actions { margin-top: 16px; display: flex; gap: 10px; flex-wrap: wrap; }
      #theme_toggle { background: rgba(255,255,255,0.12); border: 1px solid rgba(255,255,255,0.25); color: #fff; border-radius: 10px; font-weight: 600; padding: 8px 16px; }
      .layout { display: flex; gap: 18px; align-items: flex-start; }
      @media (max-width: 1100px) { .layout { flex-direction: column; } }
      .sidebar { flex: 0 0 340px; max-width: 100%; }
      .main-panel { flex: 1; min-width: 0; }
      .card { background: var(--surface); border: 1px solid var(--border); border-radius: 16px; box-shadow: var(--shadow); padding: 18px; margin-bottom: 16px; }
      .card-title { font-size: 11px; font-weight: 700; letter-spacing: 0.1em; text-transform: uppercase; color: var(--muted); margin: 0 0 14px; }
      .filter-block { background: var(--surface-2); border: 1px solid var(--border); border-radius: 12px; padding: 14px; margin-bottom: 12px; }
      .control-label { font-size: 10px !important; font-weight: 700 !important; letter-spacing: 0.08em !important; text-transform: uppercase !important; color: var(--muted) !important; margin-bottom: 6px !important; }
      .selectize-input, .form-control, .shiny-input-container select { border-radius: 10px !important; border: 1px solid var(--border) !important; min-height: 42px !important; background: var(--surface) !important; color: var(--text) !important; }
      .csv-status { font-size: 12px; color: var(--muted); margin-top: 8px; padding: 8px 10px; background: var(--surface-2); border-radius: 8px; border-left: 3px solid var(--navy); }
      #download_report { width: 100%; margin-top: 8px; padding: 14px 18px; font-size: 15px; font-weight: 700; border-radius: 12px; border: none; background: linear-gradient(135deg, #dc2626 0%, #b91c1c 100%); color: #fff; }
      .status-empty { color: var(--muted); font-size: 15px; padding: 20px 0; text-align: center; }
      .preview-card { padding: 12px; }
      .report-frame { width: 100%; height: 1180px; border: 1px solid var(--border); border-radius: 12px; background: #fff; }
      body.dark-mode { --bg: #0b1220; --surface: #101a2e; --surface-2: #0f172a; --text: #e5e7eb; --muted: #93a4c0; --border: rgba(148, 163, 184, 0.25); --shadow: 0 8px 30px rgba(0,0,0,0.35); }
    "))
  ),
  div(
    class = "app-wrap",
    div(
      class = "hero",
      h1("Hitting Report"),
      p("Build hitting reports by date and hitter — preview and download PDF"),
      div(class = "hero-actions", actionButton("theme_toggle", "Toggle dark mode"))
    ),
    div(
      class = "layout",
      div(
        class = "sidebar",
        div(
          class = "card",
          div(class = "card-title", "Data source"),
          selectInput("local_csv", "CSV in app folder", choices = c("(no CSV found)" = "")),
          fileInput("csv_upload", "Or upload CSV", accept = c(".csv")),
          div(class = "csv-status", textOutput("csv_status"))
        ),
        div(
          class = "card",
          div(class = "card-title", "Report filters"),
          div(
            class = "filter-block",
            selectInput("batter_team", "Team", choices = "All", selected = "All"),
            selectInput("game_date", "Game date", choices = NULL, multiple = TRUE),
            selectInput("batter", "Hitter", choices = NULL)
          ),
          downloadButton("download_report", "Download PDF report")
        )
      ),
      div(
        class = "main-panel",
        div(
          class = "card preview-card",
          uiOutput("report_status"),
          uiOutput("report_preview")
        )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  observeEvent(input$theme_toggle, {
    shinyjs::runjs("document.body.classList.toggle('dark-mode');")
  })

  preview_dir <- file.path(tempdir(), paste0("hitting_report_preview_", session$token))
  dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)
  shiny::addResourcePath("hitting_report_preview", preview_dir)
  session$onSessionEnded(function() {
    unlink(preview_dir, recursive = TRUE)
  })

  preview_path <- reactiveVal(NULL)
  preview_version <- reactiveVal(0)
  report_meta <- reactiveVal(list())

  input_or <- function(x, default = NULL) {
    if (is.null(x) || length(x) == 0 || identical(x, "")) default else x
  }

  refresh_local_csv_choices <- function() {
    paths <- list_local_csvs()
    if (length(paths) == 0) {
      updateSelectInput(session, "local_csv", choices = c("(no CSV in app folder)" = ""), selected = "")
      return(invisible(character(0)))
    }
    labels <- basename(paths)
    names(paths) <- labels
    current <- isolate(input$local_csv)
    selected <- if (!is.null(current) && nzchar(current) && current %in% paths) current else paths[[1]]
    updateSelectInput(session, "local_csv", choices = paths, selected = selected)
    invisible(paths)
  }

  observe({ refresh_local_csv_choices() })

  raw_df <- reactive({
    if (has_csv_upload(input$csv_upload)) {
      return(read.csv(input$csv_upload$datapath[1], stringsAsFactors = FALSE))
    }
    path <- input_or(input$local_csv, "")
    validate(need(nzchar(path) && file.exists(path), paste0("Add a .csv to ", app_dir(), " or upload one.")))
    read.csv(path, stringsAsFactors = FALSE)
  })

  output$csv_status <- renderText({
    if (has_csv_upload(input$csv_upload)) {
      paste("Using upload:", input$csv_upload$name[1])
    } else if (nzchar(input_or(input$local_csv, ""))) {
      paste("Using:", normalizePath(input$local_csv, winslash = "/"))
    } else {
      paste0("CSV folder: ", app_dir())
    }
  })

  prepared_df <- reactive({
    prepare_hitter_data(raw_df())
  })

  team_filtered_df <- reactive({
    df <- prepared_df()
    if (!identical(input_or(input$batter_team, "All"), "All")) {
      df <- df %>% filter(BatterTeam == input$batter_team)
    }
    df
  })

  observeEvent(raw_df(), {
    df <- prepared_df()
    teams <- sort(unique(stats::na.omit(df$BatterTeam)))
    team_choices <- c("All", teams)
    current <- isolate(input$batter_team)
    selected <- if (!is.null(current) && current %in% team_choices) current else {
      if ("UCO_HUS" %in% teams) "UCO_HUS" else "All"
    }
    updateSelectInput(session, "batter_team", choices = team_choices, selected = selected)
  }, ignoreInit = FALSE)

  observeEvent(input$game_date, {
    selected <- input$game_date
    if (is.null(selected) || length(selected) <= 1L) {
      return()
    }
    sel_chr <- as.character(selected)
    if (GAME_DATE_ALL %in% sel_chr && length(sel_chr) > 1L) {
      updateSelectInput(session, "game_date", selected = GAME_DATE_ALL)
    }
  }, ignoreInit = TRUE)

  observeEvent(team_filtered_df(), {
    df <- team_filtered_df()
    dates <- sort(unique(df$Date[!is.na(df$Date)]))
    if (length(dates) == 0) {
      updateSelectInput(session, "game_date", choices = character(0))
      updateSelectInput(session, "batter", choices = character(0))
      return()
    }
    selected_dates <- preserve_game_date_selection(isolate(input$game_date), dates)
    updateSelectInput(session, "game_date", choices = game_date_choices(dates), selected = selected_dates)
  }, ignoreInit = FALSE)

  observeEvent(list(input$game_date, team_filtered_df(), input$batter_team), {
    df <- team_filtered_df()
    req(input$game_date)
    dates <- sort(unique(df$Date[!is.na(df$Date)]))
    df <- filter_df_by_game_dates(df, input$game_date, dates)
    batters <- sort(unique(df$Batter))
    if (length(batters) == 0) {
      updateSelectInput(session, "batter", choices = character(0))
      return()
    }
    current <- isolate(input$batter)
    selected <- if (!is.null(current) && current %in% batters) current else batters[1]
    updateSelectInput(session, "batter", choices = batters, selected = selected)
  }, ignoreInit = FALSE)

  report_data <- reactive({
    req(input$batter, input$game_date)
    df <- team_filtered_df() %>%
      filter(Batter == input$batter)
    df <- filter_df_by_game_dates(df, input$game_date, team_filtered_df()$Date)
    validate(need(nrow(df) > 0, "No data for this hitter on the selected date(s)."))
    df
  })

  build_current_report <- function(dest) {
    df <- report_data()
    build_hitting_report_pdf(dest, df, input$batter)
    report_meta(list(
      batter = input$batter,
      date = input$game_date,
      formatted_name = format_hitter_name(input$batter)
    ))
    preview_version(preview_version() + 1)
  }

  observeEvent(
    list(input$batter, input$game_date, input$batter_team, input$local_csv, input$csv_upload),
    {
      req(input$batter, input$game_date)
      out <- file.path(preview_dir, "preview.pdf")
      tryCatch(
        {
          build_current_report(out)
          preview_path(out)
        },
        error = function(e) {
          preview_path(NULL)
          showNotification(paste("Report error:", conditionMessage(e)), type = "error", duration = 8)
        }
      )
    },
    ignoreInit = FALSE
  )

  output$report_status <- renderUI({
    path <- preview_path()
    meta <- report_meta()
    if (is.null(path) || !file.exists(path)) {
      return(div(class = "status-empty", "Choose a game date and hitter — your report will appear here."))
    }
    date_text <- game_date_status_text(meta$date)
    div(
      class = "status-empty",
      paste0(meta$formatted_name, " · ", date_text)
    )
  })

  output$report_preview <- renderUI({
    path <- preview_path()
    validate(need(!is.null(path) && file.exists(path), "Report not ready yet."))
    tags$iframe(
      class = "report-frame",
      src = paste0("hitting_report_preview/preview.pdf?v=", preview_version()),
      type = "application/pdf"
    )
  })

  output$download_report <- downloadHandler(
    filename = function() {
      meta <- report_meta()
      if (length(meta) == 0) return("Hitting Report.pdf")
      paste0(gsub("[^A-Za-z0-9 _-]", "", meta$formatted_name), " Hitting Report.pdf")
    },
    content = function(file) {
      build_current_report(file)
    }
  )
}

shinyApp(ui, server)

