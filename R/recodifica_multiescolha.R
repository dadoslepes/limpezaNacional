#' Recodifica Multiescolhas
#'
#' Função para recodificar as alternativas nas questões de múltipla escolha
#' @param variavel Vetor da variável a ser manipulada
#' @param old_cod Código como está na variável
#' @param new_cod Código como deve ficar na variável
#' @export
#' @examples
#' recodifica_multiescolha()

recodifica_multiescolha <- function( variavel, old_cod, new_cod){

  variavel <- stringr::str_replace_all(
    variavel,
    old_cod,
    new_cod
  )

  return(variavel)

}
