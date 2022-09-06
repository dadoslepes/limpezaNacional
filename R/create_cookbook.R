#' Create Cookbook
#'
#' Função para criar uma estrutura de dicionário para a base de dados oferecida.
#' @param dados Base de dados a ser utilizada
#' @export
#' @examples
#' create_cookbook()
create_cookbook <- function(dados){
  options(scipen = 999)

  dicionario <- data.frame(matrix(
    nrow = ncol(base_dados),
    ncol = 10
  ))

  names(dicionario) <- c(
    "codigo_variavel", "posicao", "tamanho",
    "tipo", "descricao_variavel", "valores",
    "descricao_valores", "missing", "moda",
    "media_frequencia"
  )

  dicionario$codigo_variavel = names(base_dados)
  dicionario$posicao  = 1:nrow(dicionario)

  for (numero_variavel in 1:nrow(dicionario)) {
    dicionario$tamanho[numero_variavel] = max(
      nchar(base_dados[,numero_variavel]),
      na.rm = T)
  }

  dicionario = dicionario %>%
    get_type(
      dicionario
    ) %>%
    get_values(
      dicionario
    )

  for (numero_variavel in 1:nrow(dicionario)) {
    dicionario$missing[numero_variavel] = (
      sum(is.na(base_dados[,numero_variavel])) /
      length(base_dados[,numero_variavel])
    ) %>%
      round(.,4)
  }

  dicionario = mode_percentual(
    base_dados =  base_dados,
    dicionario =  dicionario
  )

  return(dicionario)
}

#' mode percentual
#'
#' @param base_dados Base de dados a ser utilizada
#' @param dicionario Dicionario sendo criado
mode_percentual = function(base_dados, dicionario){

  for (numero_variavel in 1:nrow(dicionario)) {
    moda = subset(
      table( base_dados[ , numero_variavel]),
      table( base_dados[ , numero_variavel]) == max( table( base_dados[ , numero_variavel]))
    )

    if( is.integer0(moda) | is.numeric0(moda)){
      dicionario$moda[numero_variavel] = 0
    }else if( length(moda)> 1){
      dicionario$moda[numero_variavel] = ""
    }else{
      dicionario$moda[numero_variavel] =  (moda / length(base_dados[,numero_variavel])) %>%
        round(.,4)
    }
  }
  return(dicionario)
}

#' get values
#'
#' @param base_dados Base de dados a ser utilizada
#' @param dicionario Dicionario sendo criado
get_values = function(base_dados, dicionario){
  for (numero_variavel in 1:nrow(dicionario)) {
    if (length( unique( base_dados[ , numero_variavel])) >= 45){
      dicionario$valores[numero_variavel] = "muitos valores, nao cabe aqui"
    }else{
      valores_unicos = unique( na.omit( base_dados[ , numero_variavel])) %>%
        as.character(.)
      character_valores = ""
      for(i in 1:length(valores_unicos)){
        character_valores = paste(valores_unicos[i],";",character_valores)
      }
      n_last = nchar(character_valores) -3

      dicionario$valores[numero_variavel]  = substr(
        character_valores,
        start = 1 ,
        stop = n_last
      )
    }
  }
  return(dicionario)
}

#' get type
#'
#' @param base_dados Base de dados a ser utilizada
#' @param dicionario Dicionario sendo criado
get_type = function(base_dados, dicionario){
  for (numero_variavel  in 1:nrow(dicionario)){
    classe = class(base_dados[,numero_variavel])
    if( classe == "character"){
      dicionario$tipo[numero_variavel] = "string"
    }else if(classe == "numeric"){
      dicionario$tipo[numero_variavel] = "numérico"
    }else if( classe == "integer"){
      dicionario$tipo[numero_variavel] = "inteiro"
    }else if( classe == "logical"){
      dicionario$tipo[numero_variavel] = "lógico"
    }else if( classe == "complex"){
      dicionario$tipo[numero_variavel] = "complexo"
    }
  }
  return(dicionario)
}

#' check integer
#'
#' @param x Valor a ser avaliado
is.integer0 <- function(x) {
  is.integer(x) && length(x) == 0L
}

#' check numeric
#'
#' @param x Valor a ser avaliado
is.numeric0 <- function(x) {
  identical(x, numeric(0))
}
