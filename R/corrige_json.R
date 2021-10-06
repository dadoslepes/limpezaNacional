#' Corrige o JSON transformado em pela função transforma_json
#'
#' Essa função corrige algumas marcações originais do JSON transformados,
#' mantendo somente as informações que indicam qual a pergunta do questionário
#' original e qual suas informações.
#'
#'
#' @param base Base convertida pela função transforma_json
#' @param questionItem parte do nome da coluna que indica a questão do questionário. Padrão é questionItem
#' @param answer parte do nome da coluna que indica a resposta da questão. Padrão é extra.
#' @param extraQuestion parte do nome da coluna que indica resposta extra da questão. Padrão é .extraQuestion
#' @param varName nome que será dado para as colunas novas. Padrão é ed_q
#' @param varNameExtra nome que irá indicar que aquela coluna representa a resposta da questão extra de determinado item do questionário. Padrão é _extra
#' @param na indica qual a marcação de NA Padrão é 'NA'
#' @param keepExtra um vetor com os números das questões que tem pergunta extra. Padrão é NULL
#' @return Dataframe corrigido
#' @export
corrige_json_df <- function(
  base,
  questionItem = 'questionItem',
  answer = 'answer',
  extraQuestion = '.extraQuestion',
  varName = 'ed_q',
  varNameExtra = '_extra',
  na = 'NA',
  keepExtra = NULL
){

  iniciais <- base %>% dplyr::select(!tidyselect::matches('^[0-9]'))

  question_item <- base %>% dplyr::select(tidyselect::ends_with(questionItem))

  # Seleciona as respostas

  answer <- base %>% dplyr::select(tidyselect::ends_with(answer)) %>% data.frame()

  # Dá nome para as questionI

  names(answer) <- paste0(varName, question_item[1,])

  # Cria um dataframe que contém os numeros das questões do instrumento, o
  # número no json e o extraQuestion
  question2 <- data.frame(t(question_item[1,]))

  names(question2) <- 'ind_instrumento'

  question2$ind_json <- row.names(question2)

  question2$extra <- paste0(1:nrow(question2), extraQuestion)

  # retira os que não existem no ed
  question2 <- question2[question2$extra %in% names(base),]

  # Cria um dataframe somente com as perguntas extras que tem o nome do json
  df_extraQuestion <- base[,c(question2$extra)]

  # dá o nome para o dataframe considerando o nome no instrumento

  colnames(df_extraQuestion) <- paste0(
    varName,
    question2$ind_instrumento,
    varNameExtra
  )

  if(is.null(keepExtra)){
    final <- cbind(iniciais, answer)
    final <- final[,-which(names(final) %in% c(varName))]
  } else {
    df_extraQuestion <- data.frame(df_extraQuestion[
      , paste0(varName, keepExtra, varNameExtra)
    ])

    colnames(df_extraQuestion)<- paste0(varName, keepExtra, varNameExtra)

    final <- cbind(iniciais, answer, df_extraQuestion)

    # Remove as colunas que são referentes a instrução
    final <- final[,-which(names(final) %in% c(varName, paste0(varName, varNameExtra)))]

  }


  # Remove as colunas com valor de NA em questionItem
  final <- final %>% dplyr::select(!tidyselect::starts_with(paste0(varName, na)))

  final

}
