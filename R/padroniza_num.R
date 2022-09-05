#' Padroniza Números
#'
#' Função para padronizar questões numéricas abertas
#' @param base Base a ser manipulada
#' @export
#' @examples
#' padroniza_num()

padroniza_num <- function(base){

  base[base == '00' ] <- '0'
  base[base == '000' ] <- '0'
  base[base == '0000' ] <- '0'
  base[base == '01' ] <- '1'
  base[base == '02' ] <- '2'
  base[base == '03' ] <- '3'
  base[base == '04' ] <- '4'
  base[base == '05' ] <- '5'
  base[base == '06' ] <- '6'
  base[base == '07' ] <- '7'
  base[base == '08' ] <- '8'
  base[base == '09' ] <- '9'

  return(base)
}
