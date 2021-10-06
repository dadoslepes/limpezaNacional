#' Resolve problemas com data
#'
#' Essa função resolve problemas com a data caso ela venha no formato c('Dia', 'Mes', 'Ano')
#'
#'
#'
#'
#' @param base base de dados
#' @param colunas Colunas que contém as datas com problema
#' @return Dataframe corrigido
#' @export
resolve_data <- function(
  base,
  colunas
){

  for (coluna in 1:length(colunas)) {

    dataOriginal <- base[,colunas[coluna]]

    dataIntermediaria <- list()

    for (i in 1:length(dataOriginal)) {

      dataIntermediaria[[i]] <- eval(
        parse(
          text = dataOriginal[i]
        )
      )
    }

    for (i in 1:length(dataIntermediaria)) {

      dataOriginal[i] <- paste0(dataIntermediaria[[i]], collapse = '-')
    }

    base[,colunas[coluna]] <- dataOriginal
  }

  return(base)



}

