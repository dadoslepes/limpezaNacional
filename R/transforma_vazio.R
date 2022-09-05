#' Transforma Vazio
#'
#' Essa função transforma os valores NA's para um character vazio ""
#' @param base base de dados a ser utilizada
#' @export
#' @examples
#' transforma_vazio()

transforma_vazio <- function(base){

  base[is.na(base)] <- ''

  return(base)
}
