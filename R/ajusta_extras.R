#' Ajusta Extras
#'
#' Função para corrigir as questões extras com o padrão antigo
#' @param base Base de dados a ser manipulada
#' @param vetor Vetor adicional
#' @export
#' @examples
#' ajusta_extras()

ajusta_extras <- function(base, vetor=NULL){

  nomes <- names(base)[stringr::str_detect(names(base), pattern = "extra$")]

  for(nome in nomes){

    if(nome %in% vetor){
      base[,nome] <- gsub(","   , " " , base[,nome])
    } else {

      base[,nome] <- gsub(","   , "" , base[,nome])
    }

    base[,nome] <- gsub("\\[" , "" , base[,nome])
    base[,nome] <- gsub("\\]" , "" , base[,nome])
    base[,nome] <- gsub("null", "" , base[,nome])
    base[,nome] <- gsub("NA"  , "" , base[,nome])
    base[,nome] <- gsub('"'   , "" , base[,nome], fixed = TRUE)
    base[,nome] <- gsub('"'   , "" , base[,nome], fixed = TRUE)

  }

  return(base)

}


