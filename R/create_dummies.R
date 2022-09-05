#' Create Dummies
#'
#' Função para criar as variáveis dummies a partir da variável original.
#' @param dados Base de dados a ser utilizada
#' @param variavel  Nome da variável como characther
#' @param n_opcoes  Número de opções da questão que originou a variável
#' @export
#' @examples
#' create_dummies()
create_dummies <- function(dados, variavel, n_opcoes){

  dados[ ,variavel] <- gsub("\\[", "", dados[,variavel])
  dados[ ,variavel] <- gsub("\\]", "", dados[,variavel])

  # Aplicando a função dummycols que cria as dummies para os valores observados ----
  var_dummy <- as.data.frame(
    fastDummies::dummy_cols(
      dados[ , variavel],
      split = ","
    )
  )

  numeros <- c(1:n_opcoes)
  numeros[length(numeros)+1] <- "NA"

  # Criando uma lista com os nomes de todas as dummies que sao necessarias no ----
  # intervalo
  names_n_opcoes <- paste0(
    '.data_',
    numeros
  )

  data_zerada <- as.data.frame(matrix(
    data = 0,
    nrow = nrow(var_dummy),
    ncol = length(numeros)
    ))
  names(data_zerada) <- names_n_opcoes

  # Verifica se todos os valores possiveis foram encontrados, caso contrario ----
  # cria-se a dummy para o valor nao encontrado
  for (i in 1:length(numeros)){
    ifelse(
       names_n_opcoes[i] %in% names(var_dummy),
       data_zerada[ ,
         names(data_zerada) == names_n_opcoes[i]
       ] <- var_dummy[ ,
         names(var_dummy) == names_n_opcoes[i]
       ],
       NA
    )

  }

  # Trocando o nome das variáveis para adequar ao instrumento ----
  nomes_novos <- paste0(
    variavel,
    "_d",
    numeros
    # 1:n_opcoes
  )
  names(data_zerada) <- nomes_novos

  # Nas linhas com resposta NA na variavel original, troca-se os 0 nas dummies
  # por NA
  data_zerada[ data_zerada[,length(numeros)] == 1, -c(length(numeros))] <- NA_real_

  # Inserindo a dummie no dataframe ----
  dados <- cbind(
    dados,
    data_zerada
  )

  # Realocando ela para ficar ao lado da variável originária ----
  dados <- dplyr::relocate(
    .data = dados,
    (ncol(dados) - n_opcoes +1):ncol(dados),
    .after = variavel
  )

  return(dados)
}
