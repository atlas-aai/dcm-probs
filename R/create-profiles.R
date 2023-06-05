#' Create attribute profiles
#'
#' @param attributes The number of attributes.
#'
#' @export
create_profiles <- function(attributes) {
  rep(list(c(0L, 1L)), times = attributes) |>
    stats::setNames(glue::glue("att{seq_len(attributes)}")) |>
    expand.grid() |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::everything()))) |>
    dplyr::select("total", dplyr::everything()) |>
    dplyr::arrange(.data$total,
                   dplyr::desc(dplyr::across(dplyr::everything()))) |>
    dplyr::ungroup() |>
    dplyr::select(-"total") |>
    tibble::as_tibble()
}
