#' Resolve problemas com vetores concatenados
#'
#' Essa função resolve problemas com colunas que possuem vetores concatenados.
#' Um exemplo são os horários de inicio de aplicação de um instrumento. Na base
#' essas informações estão dispostas em um vetor com valores separados por virgula,
#' desta forma: "c("11", "30")". O que nós desejamos é deixar com o seguinte formato:
#' "11:30". Para isso, basta passar o argumento `collapse = ':'` dentro da função.
#'
#' Também é possível usar a função para resolver problemas com datas. Uma data disposta
#' dessa maneira: "c("08","02","2021") deverá ficar dessa maneira: "08-02-2021". Para isso,
#' basta passar o argumento `collapse = '-'`.
#'
#' Essa função roda independente do número de argumentos em cada vetor.
#'
#'
#'
#'
#'
#' @param base base de dados
#' @param colunas Colunas que contém as datas com problema
#' @param collapse Argumento separador dos valores
#' @return Dataframe corrigido
#' @export
resolve_vetores_concatenados <- function(
  base,
  colunas,
  collapse = '-'
){

  for (coluna in 1:length(colunas)) {

    vetorOriginal <- base[,colunas[coluna]]

    vetorIntermediaria <- list()

    for (i in 1:length(vetorOriginal)) {

      vetorIntermediaria[[i]] <- eval(
        parse(
          text = vetorOriginal[i]
        )
      )
    }

    for (i in 1:length(vetorIntermediaria)) {

      vetorOriginal[i] <- paste0(vetorIntermediaria[[i]], collapse = collapse)
    }

    base[,colunas[coluna]] <- vetorOriginal
  }

  return(base)



}

