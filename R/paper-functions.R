
# define functions (move these to package)

#' TITLE
#'
#' DESC
#' @param model DESC
#' @param cluster1 DESC
#' @param cluster2 DESC
#' @return DESC
#' @export
#' @importFrom sandwich estfun vcovHC sandwich
robust_vcov <- function(model, cluster1 = NULL, cluster2 = NULL)
{
  bread <- vcov(model)
  u <- sandwich::estfun(model)
  k <- model$rank
  n <- length(model$residuals)
  if (is.null(cluster1)) {
    vcov <- sandwich::vcovHC(model, type = "HC0")
  }
  else if (is.null(cluster2)) {
    n_g <- length(unique(cluster1))
    dfc <- n_g / (n_g - 1)
    u_clust <- apply(u, 2, tapply, cluster1, sum)
    vcov <- sandwich::sandwich(model, meat = crossprod(u_clust) / n)
  }
  else {
    cluster12 <- paste(cluster1, cluster2)
    n_g1 <- length(unique(cluster1))
    n_g2 <- length(unique(cluster2))
    n_g12 <- length(unique(cluster12))
    dfc1 <- n_g1 / (n_g1 - 1)
    dfc2 <- n_g2 / (n_g2 - 1)
    dfc12 <- n_g12 / (n_g12 - 1)
    u_clust1 <- apply(u, 2, tapply, cluster1, sum)
    u_clust2 <- apply(u, 2, tapply, cluster2, sum)
    u_clust12 <- apply(u, 2, tapply, cluster12, sum)
    vc1 <- dfc1 * sandwich::sandwich(model, meat = crossprod(u_clust1) / n)
    vc2 <- dfc2 * sandwich::sandwich(model, meat = crossprod(u_clust2) / n)
    vc12 <- dfc12 * sandwich::sandwich(model, meat = crossprod(u_clust12) / n)
    vcov <- vc1 + vc2 - vc12
  }
  vcov
}

#' TITLE
#'
#' DESC
#' @param model DESC
#' @param cluster1 DESC
#' @param cluster2 DESC
#' @return DESC
#' @export
robust_se <- function(model, cluster1 = NULL, cluster2 = NULL)
{
  vcov <- robust_vcov(model, cluster1, cluster2)
  se <- diag(vcov) ^ .5
  coefs <- coef(model)
  ok <- match(names(se), names(coefs))
  out <- rep(0, length(coefs))
  out[ok] <- se
  out
}

#' TITLE
#'
#' DESC
#' @param vector DESC
#' @return DESC
#' @export
capitalize <- function(vector)
{
  unname(sapply(vector, function(string) {
    string_list <- strsplit(string, " ")[[1]]
    paste(
      toupper(substring(string_list, 1, 1)),
      substring(string_list, 2),
      sep = "", collapse = " ")
  }))
}

#' TITLE
#'
#' DESC
#' @param models DESC
#' @return DESC
#' @export
fix_coef_names <- function(models)
{
  coef_names <- unique(unlist(sapply(lapply(models, coef), names)))
  coef_names <- gsub("_", " ", coef_names)
  coef_names <- gsub("\\(|\\)", "", coef_names)
  capitalize(coef_names)
}
