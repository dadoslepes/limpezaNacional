#' Ordena OD Ceará
#'
#' Função para reordenar as colunas da Observação Direta
#' @param base_od Base de dados da Observação Direta
#' @export
#' @examples
#' ordenar_od()

ordenar_ce_od <- function(base_od){

  base_od <- base_od %>%
    # reordenando variaveis dentro da base
    dplyr::select(
      formtype, geolocation, od_qa,
      nome_ue_padronizado, od_qc, od_qd,
      od_qe, od_qf, od_qg,
      od_qh, od_qi, od_qj,
      od_qk, od_ql, od_qm,
      od_qn, appliedBy, od_qo,
      od_qp, od_qq, everything(),
      od_qr
    )
  return(base_od)
}

#' Ordena EP Ceará
#'
#' Função para reordenar as colunas da Entrevista com Professores
#' @param base_ep Base de dados da Entrevista com Professores
#' @export
#' @examples
#' ordenar_ep()

ordenar_ce_ep <- function(base_ep){

  base_ep <- base_ep %>%
    # reordenando variaveis dentro da base
    dplyr::select(
      formtype, geolocation, appliedBy,
      ep_qa, nome_ue_padronizado , ep_qc, ep_qd,
      everything(), ep_q80
    )
}

#' Ordena ED Ceará
#'
#' Função para reordenar as colunas da Entrevista com Diretores
#' @param base_ed Base de dados da Entrevista com Diretores
#' @export
#' @examples
#' ordenar_ed()

ordenar_ce_ed <- function(base_ed){

  base_ed <- base_ed %>%
    # reordenando variaveis dentro da base
    dplyr::select(
      formtype, appliedBy, geolocation,
      ed_qa, nome_ue_padronizado, ed_qc, everything()
    )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q106_extra_1", "ed_q106_extra_2"),
    .after = ed_q106
  )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q107_extra_1", "ed_q107_extra_2"),
    .after = ed_q107
  )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q108_extra_1","ed_q108_extra_2"),
    .after = ed_q108
  )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q109_extra_1","ed_q109_extra_2"),
    .after = ed_q109
  )

  base_ed <- dplyr::relocate(
    base_ed,
    c("ed_q110_extra_1","ed_q110_extra_2"),
    .after = ed_q110
  )

  return(ed)
}
