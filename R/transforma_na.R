#' Transforma NA
#'
#' Função para adequar as diversas formas de valores vazios para NA
#' @param base  Base de dados a ser manipulada
#' @export
#' @examples
#' transforma_na()

transforma_na <- function(base){

  base[base == "NULL"] <- NA
  base[base == "null"] <- NA
  base[base == "None"] <- NA
  base[base == "character(0)" ] <- NA
  base[base == "integer(0)" ] <- NA
  base[base == "" ] <- NA
  base[base == 999 ] <- NA
  base[base == 9999 ] <- NA
  base[base == 99999 ] <- NA
  base[base == 999999 ] <- NA
  base[base == 9999999 ] <- NA
  base[base == 99999999 ] <- NA
  base[base == 999999999 ] <- NA
  base[base == 9999999999 ] <- NA
  base[base == 99999999999 ] <- NA
  base[base == 999999999999 ] <- NA
  base[base == 9999999999999 ] <- NA
  base[base == 99999999999999 ] <- NA
  base[base == 999999999999999 ] <- NA
  base[base == 9999999999999999 ] <- NA
  base[base == 99999999999999999 ] <- NA
  base[base == 9998 ] <- NA
  base[base == 8888 ] <- NA
  base[base == 9996 ] <- NA
  base[base == 6666 ] <- NA
  base[base == 666 ] <- NA
  base[base == 9969 ] <- NA
  base[base == 99992 ] <- NA
  base[base == 99994 ] <- NA
  base[base == "9998  " ] <- NA
  base[base == "\n9999" ] <- NA
  base[base == "9999" ] <- NA

  #questao 97 a 101
  base[base == 99989998 ] <- NA
  base[base == "9999 9999" ] <- NA
  base[base == "9998 9998" ] <- NA
  base[base == 99993 ] <- NA

  return(base)
}
