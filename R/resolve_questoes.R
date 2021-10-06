#' Resolve questões que vem dentro de um vetor
#'
#' Essa função resolve problemas com as colunas que vem dentro de um vetor
#'
#'
#'
#'
#' @param base base de dados
#' @param questoes Colunas que contém as informações com problema
#' @return Dataframe corrigido
#' @export
resolve_questoes <- function(
  base,
  questoes
){

  for (questao in 1:length(questoes)) {

    questaoOriginal <- base[,questoes[questao]]

    questaoIntermediaria <- list()

    for (i in 1:length(questaoOriginal)) {

      questaoIntermediaria[[i]] <- eval(
        parse(
          text = questaoOriginal[i]
        )
      )
    }

    for (i in 1:length(questaoIntermediaria)) {

      questaoOriginal[i] <- paste0(unlist(questaoIntermediaria[[i]]),collapse = ",")
    }

    base[,questoes[questao]] <- questaoOriginal
  }

  return(base)

}
