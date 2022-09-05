#' ajusta_categoria
#'
#' Função que corrige categoricas entre colchetes
#'
#' @param base dataset with the variables to adjust
#' @export
#' @examples
#' ajusta_categorica()

ajusta_categoricas <- function(base){

  for(numero_coluna in 1:ncol(base)){
    base[ ,numero_coluna] <- gsub("[" , "" , base[,numero_coluna], fixed = TRUE)
    base[ ,numero_coluna] <- gsub("]" , "" , base[,numero_coluna], fixed = TRUE)
  }

  return(base)
}
