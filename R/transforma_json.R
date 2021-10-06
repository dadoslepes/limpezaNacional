#' Transforma o JSON
#'
#' Essa função é responsável por transformar um arquivo formato JSON em
#' um objeto tipo dataframe.
#'
#'
#'
#' @param path Caminho para o arquivo json
#' @param convertList Se a função deverá converter as colunas que são listas em character. Padrão é TRUE.
#' @return Dataframe corrigido
#' @export
transforma_json <- function(
  path,
  convertList = T
){
  # Lê a base de dados
  json_data <- stream_in(
    file(
      path
    )
  )

  # converte para nested columns
  json_data <-  jsonlite::flatten(json_data)

  if(convertList){
    # Converte colunas que são listas para character
    i <- sapply(json_data, is.list)
    json_data[i] <- lapply(json_data[i], as.character)
  }

  return(json_data)
}

