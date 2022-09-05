#' Padroniza Nomes
#'
#' Função para padronizar os nomes das unidades educacionais e fazer o merge
#' @param vetor Vetor a ser manipulado
#' @export
#' @examples
#' padroniza_nomes()

padroniza_nomes <- function( vetor){

  vetor <- toupper(vetor)
  vetor <- gsub( "Ã", "A"  , vetor )
  vetor <- gsub( "Â", "A"  , vetor )
  vetor <- gsub( "Á", "A"  , vetor )
  vetor <- gsub( "À", "A"  , vetor )
  vetor <- gsub( "Í", "I"  , vetor )
  vetor <- gsub( "Ô", "O"  , vetor )
  vetor <- gsub( "Õ", "O"  , vetor )
  vetor <- gsub( "Ó", "O"  , vetor )
  vetor <- gsub( "É", "E"  , vetor )
  vetor <- gsub( "Ê", "E"  , vetor )
  vetor <- gsub( "Ú", "U"  , vetor )
  vetor <- gsub( "Ç", "C"  , vetor )
  vetor <- gsub( "\\.", "" , vetor )
  vetor <- gsub( "ª", ""   , vetor )
  vetor <- gsub( "º", ""   , vetor )
  vetor <- gsub( " - ", " ", vetor )
  vetor <- gsub( "  ", " " , vetor )
  vetor <- gsub( ",", ""   , vetor )
  vetor <- gsub( "*", ""   , vetor )
  vetor <- gsub( " $", ""  , vetor )
  vetor <- gsub( "  $", "" , vetor )
  vetor <- gsub( "^ ", ""  , vetor )
  vetor <- gsub( "^  ", "" , vetor )

  return(vetor)
}
