# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# POLYTOMOUS PARAMETER TABLE HELPERS ####
# Pure functions used by IRT_poly_summary_coef_reactive.
# Design contract: docs/superpowers/specs/2026-04-15-polytomous-table-redesign-design.md
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Interleave a parameter matrix with its SE matrix so columns come out as
# p1, SE(p1), p2, SE(p2), ... Both matrices must have identical column names.
.interleave_with_ses <- function(pars, ses) {
  stopifnot(identical(colnames(pars), colnames(ses)))
  out <- cbind(pars, ses)[, order(c(seq_len(ncol(pars)),
                                    seq_len(ncol(ses)))),
                          drop = FALSE]
  p_names  <- colnames(pars)
  colnames(out) <- as.vector(rbind(p_names, paste0("SE(", p_names, ")")))
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

# Build a matrix of SEs aligned to a target set of column names, pulling
# from mirt's printSE list. Missing columns become NA. For mixed-category
# fits, fold a binary item's 'b' SE into its 'b1' slot.
.ses_to_matrix <- function(fit, target_cols, fold_b_into_b1 = FALSE) {
  se_list <- coef(fit, IRTpars = TRUE, printSE = TRUE)
  se_list[["GroupPars"]] <- NULL
  mat <- do.call(rbind, lapply(seq_along(se_list), function(i) {
    se_row <- se_list[[i]]["SE", ]
    if (fold_b_into_b1 && "b" %in% names(se_row) && "b1" %in% target_cols) {
      if (!"b1" %in% names(se_row) || is.na(se_row["b1"])) {
        se_row["b1"] <- se_row["b"]
      }
    }
    out <- setNames(rep(NA_real_, length(target_cols)), target_cols)
    keep <- intersect(target_cols, names(se_row))
    out[keep] <- se_row[keep]
    out
  }))
  colnames(mat) <- target_cols
  mat
}

# For mixed poly+dich data, fold each binary item's difficulty from
# column 'b' into 'b1' and drop columns b, g, u from par_tab.
.fold_binary_pars <- function(par_tab) {
  if ("b" %in% colnames(par_tab) && "b1" %in% colnames(par_tab)) {
    b_col  <- par_tab[, "b"]
    b1_col <- par_tab[, "b1"]
    needs  <- !is.na(b_col) & is.na(b1_col)
    par_tab[needs, "b1"] <- b_col[needs]
  }
  drop_cols <- intersect(c("b", "g", "u"), colnames(par_tab))
  par_tab[, !colnames(par_tab) %in% drop_cols, drop = FALSE]
}

# Forward declarations - filled in by subsequent tasks.
build_nrm_table       <- function(fit, use_irt) stop("not yet implemented")
build_grm_table <- function(fit) {
  pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  pars <- .fold_binary_pars(pars)

  target_cols <- colnames(pars)
  ses <- .ses_to_matrix(fit, target_cols, fold_b_into_b1 = TRUE)

  .interleave_with_ses(pars, ses)
}
build_masters_table <- function(fit, model) {
  stopifnot(model %in% c("RSM", "PCM", "GPCM"))

  pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  n_items <- nrow(pars)

  if (model == "RSM") {
    return(.build_rsm_table(fit, pars))   # implemented in Task 7
  }

  # PCM / GPCM share the same shape.
  pars <- .fold_binary_pars(pars)

  b_cols <- grep("^b\\d", colnames(pars), value = TRUE)
  b_mat  <- pars[, b_cols, drop = FALSE]
  b_ses  <- .ses_to_matrix(fit, b_cols, fold_b_into_b1 = TRUE)

  if (model == "PCM") {
    a_vec   <- rep(1, n_items)
    a_ses   <- rep(NA_real_, n_items)
  } else { # GPCM
    a_vec <- if ("a" %in% colnames(pars)) pars[, "a"] else rep(NA_real_, n_items)
    a_ses <- .ses_to_matrix(fit, "a", fold_b_into_b1 = FALSE)[, "a"]
  }

  a_df <- data.frame(a = a_vec, `SE(a)` = a_ses,
                     check.names = FALSE, stringsAsFactors = FALSE)
  b_df <- .interleave_with_ses(b_mat, b_ses)
  out  <- data.frame(a_df, b_df, check.names = FALSE,
                     stringsAsFactors = FALSE)
  rownames(out) <- rownames(pars)
  out
}

# RSM-specific stub, filled in by Task 7.
.build_rsm_table <- function(fit, pars) stop("not yet implemented")
append_fit_stats <- function(tab, fit, include_infit) {
  sx2 <- tryCatch(
    itemfit(fit, na.rm = TRUE)[, c("S_X2", "df.S_X2", "p.S_X2")],
    error = function(e) NULL
  )
  if (!is.null(sx2)) {
    tab <- data.frame(tab, sx2, check.names = FALSE)
    colnames(tab)[(ncol(tab) - 2):ncol(tab)] <-
      c("SX2-value", "df", "p-value")
  }

  if (include_infit) {
    infit <- tryCatch(
      itemfit(fit, fit_stats = "infit")[, c("outfit", "infit")],
      error = function(e) NULL
    )
    if (!is.null(infit)) {
      tab <- data.frame(tab, infit, check.names = FALSE)
      colnames(tab)[(ncol(tab) - 1):ncol(tab)] <-
        c("Outfit MNSQ", "Infit MNSQ")
    }
  }
  tab
}
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
