#' Transformation de matrices d'interaction
#'
#' Les fonctions suivantes transforment des matrices contenant des
#' informations sur les interactions entre variables (ex : corrélation)
#' en d'autres matrices.
#' @name mats_interaction
NULL

#' @describeIn mats_interaction Transformation d'une matrice de corrélation
#' en une matrice de variance.
#' @param cor Matrice de corrélation (matrice)
#' @param sigmas Vecteur des écarts-types. Doit être de même taille
#' que l'ordre de `cor`.
#' @importFrom checkmate testScalar

#' @export
cor2cov <- function(cor, sigmas)
{
  if (testRownamed(cor))
    names <- rownames(cor)
  else
    names <- names(sigmas)

  if (testScalar(cor))
  {
    assertSD(sigmas)

    cov <- matrix(sigmas, nrow = 1L, ncol = 1L)
  }
  else
  {
    ## assertCorMat(cor) Vérifier les arguments
    p <- nrow(cor)
    assertSDs(sigmas, len = p)

    cov <- cor * (sigmas %*% t(sigmas))
  }

  colnames(cov) <- rownames(cov) <- names

  return(cov)
}

#' @describeIn mats_interaction Calcul de la matrice de covariance
#' d'inclusion d'individus à partir des probabilités d'inclusion du
#' second ordre.
#' @param pi2 Matrice d'inclusion du second ordre
pi2_to_covarInc <- function(pi2)
{
  pi <- diag(pi2)

  pi2 - pi %*% t(pi)
}
