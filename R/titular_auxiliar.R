#' Função Titular Auxiliar
#'
#' Função para separar a variável referência em análise de professores titulares e auxiliares
#' @param base Base de dados a ser manipulada
#' @param col Nome da variável, como character, a ser manipulada
#' @param extra Vetor de nome das variáveis adicionais, caso seja necessário
#' @export
#' @examples
#' titular_auxiliar()

titular_auxiliar <- function(base, col, extra){

  string <- unique(
    unlist(
      stringr::str_split( base[ , col], ',')
    )
  )

  extra_string <- paste0( extra, '_', string)

  base[ , extra_string[ 1:2]] <- NA



  # LOOP

  for ( i in 1:nrow( base)) {

    if( length( unlist( stringr::str_split( base[ i, col], ','))) == 1){
      base[
        i,
        paste0(
          extra,
          '_',
          unlist( stringr::str_split( base[ i, col], ','))
        )
      ] <- unlist( stringr::str_split( base[ i, extra], ' '))[1]
    } else {
      base[
        i,
        paste0(
          extra,
          '_',
          unlist( stringr::str_split( base[ i, col], ','))[1]
        )
      ] <- unlist( stringr::str_split( base[ i, extra], ' '))[1]

      base[
        i,
        paste0(
          extra,
          '_',
          unlist( stringr::str_split( base[ i, col], ','))[2]
        )
      ] <- unlist( stringr::str_split( base[ i, extra], ' '))[2]
    }
  }

  return(base)

}
