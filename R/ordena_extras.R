#' Realocar Colunas
#'
#' Função que reordena as variáveis extras do instrumento
#' @param base Base de dados a ser manipulada
#' @export
#' @examples
#' realocar_colunas()

realocar_colunas <- function( base){

  nomes <- names(base)[ stringr::str_detect( names(base), pattern = "extra$")]

  for(nome_coluna in nomes)

    base <- base %>%
    dplyr::relocate(
      nome_coluna,
      .after = stringr::str_sub(nome_coluna, end = -7)
    )

  return(base)
}
