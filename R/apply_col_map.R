#' Apply Column Mapping and Convert Types
#'
#' This function renames columns in a dataframe based on a predefined mapping and,
#' optionally, applies type conversions to standard variables.
#'
#' @param df A dataframe whose columns need renaming.
#' @param convert_type Logical. If TRUE, converts variable types according to predefined standards. Defaults to TRUE.
#' @param clean Logical. If TRUE, uses janitor package to clean col names. Defaults to TRUE.
#' @return A dataframe with renamed columns and, optionally, converted types.
#' @examples
#' df <- data.frame(DASY_KEY = 2022, DDST_DIST_NUMBER = 1234)
#' apply_col_map(df, convert_type = TRUE)
#' @export

apply_col_map <- function(df, convert_type = TRUE, clean = TRUE) {

  # Lowercase all column names for consistency **before** renaming
  names(df) <- tolower(names(df))

  # Define a named character vector mapping new names (names) to current names (values)
  rename_map <- c(
    year = "dasy_key",
    year = "report_year",
    year_detail = "detail_dasy_key",
    dist = "ddst_dist_number",
    dist = "dist_number",
    distname = "ddst_district_name",
    schname = "dsch_school_name",
    sch  = "dsch_school_number",
    grd = "dgrd_code",
    grd_simp = "grade_simple",
    grd_low = "dsch_grade_span_low",
    grd_high = "dsch_grade_span_high",
    test = "test_name",
    sub = "subject",
    emh = "demh_emh_code",
    emh = "emh_code",
    mss = "ach_mean_ss",
    mss = "mean_ss",
    mgp = "gro_median_sgp",
    pct_pts = "pct_pts_earn",
    pct_pts_w = "pct_pts_earn_weighted",
    aec = "alt_ed_campus_yn",
    cat_simp = "category_simple",
    iep = "diep_code"
  )

  # Filter rename_map to include only columns that exist in df
  existing_renames <- rename_map[rename_map %in% names(df)]

  # Rename using the static rename_map first
  if (length(existing_renames) > 0) {
    df <- dplyr::rename(df, !!!existing_renames)
  }

  # Now remove the `_yn` suffix dynamically from all matching columns
  yn_cols <- names(df)[grepl("_yn$", names(df), ignore.case = TRUE)]
  yn_rename_map <- stats::setNames(yn_cols, sub("_yn$", "", yn_cols, ignore.case = TRUE))

  # Apply the _yn rename
  if (length(yn_rename_map) > 0) {
    df <- dplyr::rename(df, !!!yn_rename_map)
  }

  # Fix non-UTF characters in `clock_awards` if it exists
  if ("clock_awards" %in% names(df)) {
    df$clock_awards <- iconv(df$clock_awards, from = "latin1", to = "UTF-8", sub = "")
    df$clock_awards <- gsub("\u0092", "'", df$clock_awards)
  }

  # Optionally convert variable types if convert_type is TRUE
  if (convert_type) {
    # Define standard variable names for conversion
    integer_vars <- c("scale_score", "sgp", "year")
    numeric_vars <- c("mss", "mgp", "pct_pts", "pct_pts_w")
    #factor_vars  <- c("rating")
    char_vars    <- c("dist","sch")

    # Determine which of these variables exist in the dataframe
    integer_vars_exist <- intersect(integer_vars, names(df))
    numeric_vars_exist <- intersect(numeric_vars, names(df))
    #factor_vars_exist  <- intersect(factor_vars, names(df))
    char_vars_exist    <- intersect(char_vars, names(df))

    # Apply conversions using dplyr's across function
    df <- df |>
      dplyr::mutate(dplyr::across(dplyr::all_of(numeric_vars_exist), as.numeric)) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(integer_vars_exist), as.integer)) |>
      #dplyr::mutate(dplyr::across(dplyr::all_of(factor_vars_exist), as.factor)) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(char_vars_exist), as.character))
  }

  if (clean) {
    df <- janitor::clean_names(df)
  }

  return(df)
}
