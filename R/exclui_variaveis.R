#' Exclui Variáveis
#'
#' Função que exclui colunas que não continuarão na base final.
#' Essas variáveis estão sendo excluídas pois não serão necessárias nas análises
#' das respostas dos instrumentos, sendo importantes apenas no processo de
#' validação e acompanhamento da aplicação dos instrumentos.
#'
#' starttime : Horário de abertura do instrumento no aplicativo de aplicação;
#' endtime : Horário de fechamento do instrumento no aplicativo de aplicação;
#' uploaddate : Data de aplicação do instrumento no aplicativo de aplicação;
#' applicationdate : Data da aplicação do instrumento pelos aplicadores;
#' status : Indica se a observação está válida ou foi deletada;
#' justification.deletedBy : Indica qual pessoa indicou o deletamento da observação;
#' justification.reason : Justificativa do deletamento da observação por parte da coordenação do campo de aplicação
#' @param base Base de dados a ser manipulada
#' @param vetor_colunas Vetor com nome de variáveis adicionais a serem excluídas
#' @export
#' @examples
#' exclui_variaveis()

exclui_variaveis <- function(base, vetor_colunas){

  variaveis_aplicativo <- c(
    "starttime",
    "endtime",
    "uploaddate",
    "status",
    "justification.deletedBy",
    "justification.reason"
  )

  base <- base[ ,
    !( names( base ) %in% variaveis_aplicativo) &
    !( names( base ) %in% vetor_colunas)
  ]

  return(base)
}
