# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# POLYTOMOUS PARAMETER TABLE HELPERS ####
# Pure functions used by IRT_poly_summary_coef_reactive.
# Design contract: docs/superpowers/specs/2026-04-15-polytomous-table-redesign-design.md
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forward declarations - filled in by subsequent tasks.
build_nrm_table       <- function(fit, use_irt) stop("not yet implemented")
build_grm_table       <- function(fit) stop("not yet implemented")
build_masters_table   <- function(fit, model) stop("not yet implemented")
append_fit_stats      <- function(tab, fit, include_infit) stop("not yet implemented")
italicise_colnames <- function(tab) {
  skip <- c("SX2-value", "df", "p-value", "Outfit MNSQ", "Infit MNSQ")
  nms <- colnames(tab)
  for (i in seq_along(nms)) {
    if (nms[i] %in% skip) next
    if (grepl("^SE\\((.+)\\)$", nms[i])) {
      inner <- sub("^SE\\((.+)\\)$", "\\1", nms[i])
      nms[i] <- paste0("SE(\\(\\mathit{", inner, "}\\))")
    } else {
      nms[i] <- paste0("\\(\\mathit{", nms[i], "}\\)")
    }
  }
  colnames(tab) <- nms
  tab
}

strip_katex <- function(tab) {
  nms <- colnames(tab)
  nms <- gsub("^SE\\(\\\\\\(\\\\mathit\\{(.+)\\}\\\\\\)\\)$",
              "SE(\\1)", nms)
  nms <- gsub("^\\\\\\(\\\\mathit\\{(.+)\\}\\\\\\)$", "\\1", nms)
  colnames(tab) <- nms
  tab
}
rsm_threshold_ses_delta <- function(fit) stop("not yet implemented")
