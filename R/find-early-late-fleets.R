#' Identify Early/Late Fleet Pairs by Fuzzy Name Matching
#'
#' Given a character vector of fleet names, finds fleets that represent the
#' same fishery but differ only by an `early` or `late` temporal tag, using
#' fuzzy string matching to accommodate minor naming inconsistencies (e.g.
#' `OSDWLL` vs `OSDWCOLL`).
#'
#' @param fleets Character vector of fleet names. Names are expected to follow
#'   the convention `F{n}_{fleet_name}_{early|late}`, e.g.
#'   `"F1_JPN_WCNPO_OSDWLL_late_Area1"`. The numeric `F{n}_` prefix and the
#'   `early`/`late` token are stripped before matching. Fleets without an
#'   `early` or `late` tag are ignored.
#' @param max_dist Non-negative integer. Maximum edit distance (via
#'   [utils::adist()]) allowed between two base fleet names for them to be
#'   considered a match. Increase to tolerate greater name variation; decrease
#'   to require closer matches. Default: `3`.
#'
#' @return A named list. Each element corresponds to a matched early/late pair,
#'   where:
#'   * The **name** is the canonical base fleet name (the shortest name in the
#'     group, with the `F{n}_` prefix and `early`/`late` token removed).
#'   * The **value** is a character vector of the original fleet names from
#'     `fleets` that belong to the pair, sorted alphabetically.
#'
#'   Returns an empty list if no early/late pairs are found.
#'
#'
#' @seealso [utils::adist()] for the underlying edit-distance computation,
#'   [FleetNames()] to extract fleet names from an OM object.
#'
#' @export
FindEarlyLateFleets <- function(fleets, max_dist = 3) {
  stripped <- sub("^F\\d+_", "", fleets)
  base <- sub("_(early|late)(?=_|$)", "", stripped, perl = TRUE)
  has_tag <- stripped != base
  tagged_idx <- which(has_tag)
  if (length(tagged_idx) < 2) return(list())
  tagged_bases <- base[tagged_idx]
  n <- length(tagged_bases)
  area_tag <- function(x) {
    m <- regmatches(x, regexpr("_Area\\d+$", x))
    if (length(m)) m else ""
  }
  area_tags <- vapply(tagged_bases, area_tag, character(1))
  base_no_area <- sub("_Area\\d+$", "", tagged_bases)
  dists <- utils::adist(base_no_area)
  diag(dists) <- NA
  same_area <- outer(area_tags, area_tags, "==")
  diag(same_area) <- NA
  eligible <- dists <= max_dist & same_area
  pairs <- which(eligible & lower.tri(eligible), arr.ind = TRUE)
  if (nrow(pairs) == 0) return(list())
  parent <- seq_len(n)
  find <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }
  union <- function(a, b) {
    ra <- find(a); rb <- find(b)
    if (ra != rb) parent[ra] <<- rb
  }
  for (k in seq_len(nrow(pairs)))
    union(pairs[k, 1], pairs[k, 2])
  roots  <- vapply(seq_len(n), find, integer(1))
  groups <- split(seq_len(n), roots)
  groups <- groups[lengths(groups) > 1]
  if (length(groups) == 0) return(list())
  out <- list()
  for (grp in groups) {
    grp_bases <- tagged_bases[grp]
    grp_idx   <- tagged_idx[grp]
    canonical <- grp_bases[which.min(nchar(grp_bases))]
    pairwise      <- utils::adist(sub("_Area\\d+$", "", grp_bases))
    diag(pairwise) <- NA
    if (any(pairwise > 2 * max_dist, na.rm = TRUE))
      cli::cli_abort(c(
        "x" = "Ambiguous early/late fleet matches in group {.val {grp_bases}}.",
        "i" = "These fleet names are too dissimilar to match confidently.",
        "i" = "Consider adjusting {.arg max_dist} (currently {max_dist})."
      ))
    out[[canonical]] <- sort(fleets[grp_idx])
  }
  out
}
