#' Resolve questões extras com problema
#'
#' Transforma as informações respondidas nas questões extras em um vetor de texto
#' @param base base de dados
#' @param extras Colunas que contém as informações com problema
#' @return Dataframe corrigido
#' @export
resolve_extra <- function(
  base,
  extras
){

  for (extra in 1:length(extras)) {

    extraOriginal <- base[,extras[extra]]

    for (i in 1:length(extraOriginal)) {

      extraIntermediaria <- strsplit(extraOriginal[i], split = "extraAnswer")
      extraIntermediaria <- extraIntermediaria[[1]][2]
      extraOriginal[i] <- extraIntermediaria %>%
        stringr::str_remove_all(fixed("= c(")) %>%
        stringr::str_remove_all(fixed(" NA")) %>%
        stringr::str_remove_all(fixed(")")) %>%
        stringr::str_remove_all(fixed("\\n")) %>%
        stringr::str_remove_all(fixed(",")) %>%
        stringr::str_remove_all(fixed('"')) %>%
        stringr::str_trim()
    }

    base[,extras[extra]] <- extraOriginal

  }

  return(base)

}
