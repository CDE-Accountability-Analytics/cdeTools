#' Create an HTML–formatted transition matrix of before→after ratings
#'
#' @param df             A data frame containing at least the columns indicated by `pre_col` and `post_col`, plus `test` and `cat_simp`.
#' @param test           Optional string. If non‐NULL, only rows with `test == test` are used.
#' @param subgroup       Optional string. If non‐NULL, only rows with `cat_simp == subgroup` are used.
#' @param pre_col        Name of the “before” rating column (ordered factor). Default `"rating_pre"`.
#' @param post_col       Name of the “after”  rating column (ordered factor). Default `"rating_post"`.
#' @param pre_label      Label for the first (row) header. Default `NULL` → falls back to `pre_col`.
#' @param post_label     Label for the top header spanning rating columns. Default `NULL` → falls back to `post_col`.
#' @param return_df      Logical; if `TRUE`, returns the raw transition data frame instead of rendering an HTML table. Default `FALSE`.
#' @param table_title    Optional string; if non‐NULL, used verbatim as the table caption. Default `NULL` → auto–generated.
#' @param table_subtitle Optional string; if non‐NULL, rendered under the title in smaller text.
#' @return If `return_df = FALSE`: an HTML kable of the colored transition matrix with custom labels and subtitle.  
#'         If `return_df = TRUE`: a plain `data.frame` of counts with first column named as `pre_label`.
#' @importFrom dplyr filter
#' @importFrom tibble rownames_to_column
#' @importFrom knitr kable
#' @importFrom kableExtra cell_spec add_header_above kable_styling
#' @importFrom stats setNames
#' @export
trans_matrix <- function(df,
                         test           = NULL,
                         subgroup       = NULL,
                         pre_col        = "rating_pre",
                         post_col       = "rating_post",
                         pre_label      = NULL,
                         post_label     = NULL,
                         return_df      = FALSE,
                         table_title    = NULL,
                         table_subtitle = NULL) {

  data <- df
  if (!is.null(test))     data <- data |> dplyr::filter(test     == !!test)
  if (!is.null(subgroup)) data <- data |> dplyr::filter(cat_simp == !!subgroup)
  if (nrow(data) == 0) {
    stop("No data found",
         if (!is.null(test))     paste0(" for test=", test),
         if (!is.null(subgroup)) paste0(" and subgroup=", subgroup))
  }

  # Determine labels
  if (is.null(pre_label))  pre_label  <- pre_col
  if (is.null(post_label)) post_label <- post_col

  # 1) Build raw counts table
  tbl <- table(data[[pre_col]], data[[post_col]])
  mat <- as.data.frame.matrix(tbl, stringsAsFactors = FALSE)
  mat <- tibble::rownames_to_column(mat, var = pre_label)

  if (return_df) return(mat)

  # 2) Color-format each cell
  levels_before <- levels(data[[pre_col]])
get_color <- function(b, a) {
  i0 <- match(b, levels_before)
  i1 <- match(a, levels_before)
  if      (i1 < i0) "#c6efce"  else  # light green
  if      (i1 > i0) "#ffc7ce"  else  # light red
                      "#e0e0e0"       # light grey
}
  mat_fmt <- mat
  for (i in seq_len(nrow(mat_fmt))) {
    before <- mat_fmt[[pre_label]][i]
    for (j in seq(2, ncol(mat_fmt))) {
      after <- colnames(mat_fmt)[j]
      v     <- mat_fmt[i, j]
      mat_fmt[i, j] <-
        if (v == 0 || before == "-") as.character(v) else
          kableExtra::cell_spec(
            v,
            color      = "black",
            background = get_color(before, after),
            bold       = TRUE
          )
    }
  }

# 3) Build caption + subtitle
main_caption <- if (!is.null(table_title)) table_title else
  paste0(
    "Transition Matrix",
    if (!is.null(test))     paste0(" for test=", test),
    if (!is.null(subgroup)) paste0(" and subgroup=", subgroup)
  )

# wrap in left-aligned div
caption_core <- if (!is.null(table_subtitle)) {
  paste0(
    main_caption,
    "<br/><span style='font-size:0.9em;'>", table_subtitle, "</span>"
  )
} else main_caption
caption_html <- paste0(
  "<div style='text-align:left;'>",
    caption_core,
  "</div>"
)

# 4) Alignment vector: left for the first col, center for the rest
align_vec <- c("l", rep("c", ncol(mat_fmt) - 1))

# 5) Render HTML table
knitr::kable(
  mat_fmt,
  format     = "html",
  escape     = FALSE,
  align      = align_vec,
  caption    = caption_html,
  table.attr = 'style="width:auto; margin:1em auto;"'
) |>
  kableExtra::add_header_above(
    setNames(c(1, ncol(mat_fmt) - 1),
             c(" ", post_label))
  ) |>
  kableExtra::kable_styling(
    bootstrap_options = c("striped","condensed","responsive"),
    full_width       = FALSE,
    position         = "center"
  )

}


#' Summarize rating changes (before→after) for one test/subgroup
#'
#' @param df             A data frame containing at least the columns indicated by `pre_col` and `post_col`, plus `test` and `cat_simp`.
#' @param test           Optional string to filter on `test`.
#' @param subgroup       Optional string to filter on `cat_simp`.
#' @param pre_col        Name of the “before” rating column. Default `"rating_pre"`.
#' @param post_col       Name of the “after”  rating column. Default `"rating_post"`.
#' @param debug          Logical; if `TRUE`, prints any rows where change is `NA`.
#' @param table_title    Optional string; if non‐NULL, used verbatim as the table caption.
#' @param table_subtitle Optional string; if non‐NULL, rendered under the title in smaller text.
#' @return An HTML table (a `kable`) with counts & percents by change category and optional subtitle.
#' @importFrom dplyr filter mutate count arrange
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @export
trans_sum <- function(df,
                      test           = NULL,
                      subgroup       = NULL,
                      pre_col        = "rating_pre",
                      post_col       = "rating_post",
                      debug          = FALSE,
                      table_title    = NULL,
                      table_subtitle = NULL) {

  data <- df
  if (!is.null(test))     data <- data |> dplyr::filter(test     == !!test)
  if (!is.null(subgroup)) data <- data |> dplyr::filter(cat_simp == !!subgroup)
  if (nrow(data) == 0) {
    stop("No data found",
         if (!is.null(test))     paste0(" for test=", test),
         if (!is.null(subgroup)) paste0(" and subgroup=", subgroup))
  }

  # 1) Compute changes
  df2 <- data |>
    dplyr::mutate(
      diff = ifelse(
        as.character(.data[[pre_col]]) == "-", NA_real_,
        as.numeric(.data[[post_col]]) - as.numeric(.data[[pre_col]])
      ),
      change = dplyr::case_when(
        .data[[pre_col]] == "-" & .data[[post_col]] != "-" ~ "Previously Unrated",
        .data[[pre_col]] == "-" & .data[[post_col]] == "-" ~ "No Rating",
        .data[[pre_col]] != "-" & .data[[post_col]] == "-" ~ "Now Unrated",
        diff <  0 ~ paste("Up", abs(diff)),
        diff == 0 ~ "No Change",
        diff >  0 ~ paste("Down", diff),
        TRUE       ~ NA_character_
      )
    )

  if (debug) {
    nas <- df2 |> dplyr::filter(is.na(change))
    if (nrow(nas) > 0) {
      message("Rows with NA change:")
      print(nas)
    } else message("No NA change rows.")
  }

  # 2) Summarize
  summary_df <- df2 |>
    dplyr::count(change) |>
    dplyr::arrange(change) |>
    dplyr::mutate(Percent = paste0(round(n / sum(n) * 100, 1), "%")) |>
    dplyr::rename(Change = change, Count = n)

  # 3) Build captions
  main_caption <- if (!is.null(table_title)) table_title else
    paste0(
      "Transition Summary",
      if (!is.null(test))     paste0(" for test=", test),
      if (!is.null(subgroup)) paste0(" and subgroup=", subgroup)
    )
  caption_html <- if (!is.null(table_subtitle)) {
    paste0(
      main_caption,
      "<br/><span style='font-size:0.9em;'>", table_subtitle, "</span>"
    )
  } else main_caption

  # 4) Alignment: left for Change, center for counts/percents
  align_vec <- c("l", rep("c", ncol(summary_df) - 1))

# 5) Render HTML summary table
tab <- knitr::kable(
  summary_df,
  format     = "html",
  escape     = FALSE,
  align      = align_vec,
  caption    = caption_html,
  table.attr = 'style="width:auto; margin:1em auto;"'
) |>
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed", "responsive"),
    full_width       = FALSE,
    position         = "left"
  )

# Apply row colors
for (i in seq_len(nrow(summary_df))) {
  change_val <- summary_df$Change[i]

  if (grepl("^Up", change_val) | change_val == "Previously Unrated") {
    tab <- tab |> kableExtra::row_spec(i, background = "#c6efce") # light green
  } else if (grepl("^Down", change_val) | change_val == "Now Unrated") {
    tab <- tab |> kableExtra::row_spec(i, background = "#ffc7ce") # light red
  }
}

tab
}
