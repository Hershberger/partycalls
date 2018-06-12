
#' Regress a single roll call on party indicator and ideal points
#'
#' To be used inside a call to code_party_calls_1step. This function is used to
#' perform a number of tasks in the purpose of determining the influence of the
#' party on a given vote. This is accomplished by regressing votes on members'
#' party affiliation and ideology. Before this happens, however, all votes which
#' either received only `yeas' and `nays' are assigned non-values in place of
#' estimates. Further, votes which strictly follow party lines are assigned
#' estimates which guarantee they will be coded as party calls under any model
#' specification to avoid separation in the model. Finally, the votes left are
#' either put through a bias-reduced logit or ordinary least squares model as
#' per user selected parameters. The default setting of the function is to use
#' the bias-reduced logit.
#' Internal function for code_party_calls, only used if
#' use_classification_distance is FALSE.
#' @param .SD subset of a data.table of roll call votes, with a column for party
#' labels
#' @param type character string, one of brglm, glm, lm, or lasso;
#' which function to use for roll call-by-roll call regression
#' @return list of coefficient, standard error, t value, and p value for the
#' coefficient on party
#' @export
#' @importFrom brglm brglm
#' @importFrom glmnet glmnet cv.glmnet
test_rollcall <- function(.SD, type = c("brglm", "lm", "glm", "lasso"))
{
  .SD <- .SD[party %in% c("D", "R")]
  n_yea_reps <- .SD[, sum(y == 1 & party == "R", na.rm = TRUE)]
  n_nay_reps <- .SD[, sum(y == 0 & party == "R", na.rm = TRUE)]
  n_yea_dems <- .SD[, sum(y == 1 & party == "D", na.rm = TRUE)]
  n_nay_dems <- .SD[, sum(y == 0 & party == "D", na.rm = TRUE)]
  party_line_vote <-
    (n_yea_reps == 0 & n_nay_reps >  0 & n_yea_dems >  0 & n_nay_dems == 0) |
    (n_yea_reps >  0 & n_nay_reps == 0 & n_yea_dems == 0 & n_nay_dems >  0)
  if (mean(.SD[, y], na.rm = TRUE) %in% c(0:1, NaN) |
      length(unique(.SD[!is.na(y) & party %in% c("D", "R"), party])) == 1L) {
    if (type != "lasso") {
      list(b = 0, se = 0, t = Inf, p = NA_real_)
    } else {
      matrix(c(0, 1, 0), 3, 1)[2, 1]
    }
  } else if (party_line_vote) {
    if (type != "lasso") {
      list(b = 1, se = 0, t = Inf, p = 0)
    } else {
      matrix(c(0, 1, 0), 3, 1)[2, 1]
    }
  } else {
    if (type == "brglm") {
      m <- brglm::brglm(y ~ party + x, data = .SD, family = binomial)
      suppressWarnings(summ <- summary(m)$coef["partyR", ])
      list(b = summ["Estimate"], se = summ["Std. Error"],
        t = summ["z value"], p = summ["Pr(>|z|)"])
    } else if (type == "glm") {
      suppressWarnings(m <- glm(y ~ party + x, data = .SD, family = binomial))
      suppressWarnings(summ <- summary(m)$coef["partyR", ])
      list(b = summ["Estimate"], se = summ["Std. Error"],
        t = summ["z value"], p = summ["Pr(>|z|)"])
    } else if (type == "lm") {
      m <- lm(y ~ party + x, data = .SD)
      suppressWarnings(summ <- summary(m)$coef["partyR", ])
      list(b = summ["Estimate"], se = summ["Std. Error"],
        t = summ["t value"], p = summ["Pr(>|t|)"])
    } else if (type == "lasso") {
      y <- .SD[, y]
      x <- .SD[, as.matrix(cbind(as.numeric(party == "R"), x))]
      ok <- !is.na(y)
      lambda.1se <- tryCatch(glmnet::cv.glmnet(x = x[ok, ], y = y[ok],
        family = "binomial", penalty.factor = c(1, 0))$lambda.1se,
        warning = function(w) NA, error = function(e) NA, finally = NA)
      if (!is.na(lambda.1se)) {
        m <- glmnet::glmnet(x = x[ok, ], y = y[ok],
          family = "binomial", penalty.factor = c(1, 0),
          lambda = lambda.1se)
        as.matrix(coef(m))[2, 1]
      } else {
        matrix(c(0, 0, 0), 3, 1)[2, 1]
      }
    }
  }
}
